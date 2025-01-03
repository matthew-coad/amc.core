#' Goddard Institute for Space Studies (GISS) data source
#'
#' @export
#'
#' @examples
#' giss
giss <- new_amc_datasource(
  "giss",
  "Goddard Institute for Space Studies (GISS)",
  reference_code = "GISS",
  reference_url = "https://data.giss.nasa.gov/"
)

#' GISS Zonal Annual Mean Temperature Anomaly
#'
#' @export
#'
#' @examples
#' giss_ta_zonal_yearly
giss_ta_zonal_yearly <- new_amc_dataset(
  giss,
  "giss_ta_zonal_yearly",
  "GISS, Land+Sea Surface Temperature Anomaly, Zonal, Yearly, Years 1880-Present",
  "GISS Combined Land-Surface Air and Sea-Surface Water Temperature Anomalies, Zonal annual means",
  reference_url = "https://data.giss.nasa.gov/gistemp/"
)

giss_ta_zonal_yearly_filename <- "ZonAnn.Ts+dSST.csv"
giss_ta_zonal_yearly_url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv"

#' Download met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Datasets
download_giss_ta_zonal_yearly <- function() {
  giss_ta_zonal_yearly_path <- get_amc_dataset_path(giss_ta_zonal_yearly, giss_ta_zonal_yearly_filename)
  utils::download.file(url = giss_ta_zonal_yearly_url, destfile = giss_ta_zonal_yearly_path, mode = "wb")
}

giss_ta_zonal_yearly$download <- download_giss_ta_zonal_yearly

#' Read GISS Zonal Annual Mean Temperature Anomaly
read_giss_ta_zonal_yearly <- function() {
  temp_path <- get_amc_dataset_path(giss_ta_zonal_yearly, "ZonAnn.Ts+dSST.csv")
  col_types <-
    readr::cols(
      Year = readr::col_integer(),
      Glob = readr::col_character(),
      NHem = readr::col_character(),
      SHem = readr::col_character(),
      `24N-90N` = readr::col_character(),
      `24S-24N` = readr::col_character(),
      `90S-24S` = readr::col_character(),
      `64N-90N` = readr::col_character(),
      `44N-64N` = readr::col_character(),
      `24N-44N` = readr::col_character(),
      `EQU-24N` = readr::col_character(),
      `24S-EQU` = readr::col_character(),
      `44S-24S` = readr::col_character(),
      `64S-44S` = readr::col_character(),
      `90S-64S` = readr::col_character()
    )
  as_temp <- function(x) suppressWarnings(ifelse(x != '*****', as.double(x), NA))
  temp_raw <- readr::read_csv(temp_path, col_types = col_types)
  temp_long <- tidyr::pivot_longer(temp_raw, !"Year", names_to = "zone", values_to = "temperature_anomoly")
  zone_map <- tibble::tibble(
    zone = c("Glob", "NHem", "SHem"),
    zone_type_code = c(global_zone_type_code, hemisphere_zone_type_code, hemisphere_zone_type_code),
    zone_code = c(global_zone_code, northern_hemisphere_zone_code, southern_hemisphere_zone_code)
  )
  temp_long |>
    dplyr::inner_join(zone_map, by = "zone") |>
    dplyr::mutate(
      temperature_anomaly = as_temp(.data$temperature_anomoly)
    ) |>
    dplyr::select(
      "zone_type_code",
      "zone_code",
      year = "Year",
      "temperature_anomaly"
    )
}

giss_ta_zonal_yearly$read <- read_giss_ta_zonal_yearly
