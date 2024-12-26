#' Gistemp datasource code
#'
#' @export
#'
#' @examples
#' gistemp_datasource_code
gistemp_datasource_code <- "gistemp"

#' Goddard Institute for Space Studies, GISS Surface Temperature Analysis amc data source
#'
#' @export
#'
#' @examples
#' gistemp_datasource
gistemp_datasource <- new_amc_datasource(
  gistemp_datasource_code,
  "GISS Surface Temperature Analysis (GISTEMP v4)",
  "https://data.giss.nasa.gov/gistemp/"
)

#' Gistemp Zonal annual means
#'
#' @export
#'
#' @examples
#' gistemp_zonal_annual_means_dataset_code
gistemp_zonal_annual_means_dataset_code <- "zonal_annual_means"

#' Zonal Annual Mean Temperature Anomaly, 1880-present
#'
#' @export
#'
#' @examples
#' gistemp_zonal_annual_means_dataset
gistemp_zonal_annual_means_dataset <- new_amc_dataset(
  gistemp_datasource_code,
  gistemp_zonal_annual_means_dataset_code,
  "Zonal Annual Mean Temperature Anomaly, 1880-present"
)

#' List all datasets available from the gistemp datasource
#'
#' @return Dataset tibble
#' @export
#'
#' @examples
#' list_gistemp_dataset()
list_gistemp_dataset <- function() {
  gistemp_zonal_annual_means_dataset
}

#' Read gistemp zonal annual means
#'
#' @return Gistemp zonal annual means
#' @export
#'
#' @examples
#' read_gistemp_zonal_annual_means()
read_gistemp_zonal_annual_means <- function() {
  temp_path <- get_amc_dataset_path(gistemp_zonal_annual_means_dataset, "ZonAnn.Ts+dSST.csv")
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
  temp_long <- tidyr::pivot_longer(temp_raw, !.data$Year, names_to = "zone", values_to = "temperature_anomoly")
  zone_map <- tibble::tibble(
    zone = c("Glob", "NHem", "SHem"),
    zone_type_code = c(global_zone_type_code, hemisphere_zone_type_code, hemisphere_zone_type_code),
    zone_code = c(global_zone_code, northern_hemisphere_zone_code, southern_hemisphere_zone_code)
  )
  temp_long |>
    dplyr::inner_join(zone_map, by = "zone") |>
    dplyr::mutate(
      datasource_code = gistemp_datasource_code,
      dataset_code = gistemp_zonal_annual_means_dataset_code,
      temperature_anomaly = as_temp(.data$temperature_anomoly)
    ) |>
    dplyr::select(
      .data$datasource_code,
      .data$dataset_code,
      .data$zone_type_code,
      .data$zone_code,
      year = .data$Year,
      .data$temperature_anomaly
    )
}
