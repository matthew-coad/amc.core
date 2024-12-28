#' Temperature Record Website Datasource
#'
#' @export
#'
#' @examples
#' tempr_datasource
tempr_datasource <- new_amc_datasource(
  "tempr",
  "Temperature Record Website",
  "https://www.temperaturerecord.org/"
)

#' TEMPR 2,000 Year Northern Hemisphere Temperature Anomaly Temperature Reconstruction data set
#'
#' @export
#'
#' @examples
#' tempr_nhtr_dataset
tempr_nhtr_dataset <- new_amc_dataset(
  tempr_datasource,
  "tempr_nhtr",
  "TEMPR Northern Hemisphere Temperature Anomaly Temperature Reconstruction, 0AD to 1880AD"
)

tempr_nhtr_filename <- "nhtemp-moberg2005.txt"
tempr_nhtr_reference_url <- "https://www.temperaturerecord.org/"
tempr_nhtr_url <- "https://www.climatelevels.org/files/nhtemp-moberg2005.txt"

download_tempr_nhtr_dataset <- function() {
  tempr_nhtr_filepath <- get_amc_dataset_path(tempr_nhtr_dataset, tempr_nhtr_filename)
  utils::download.file(url = tempr_nhtr_url, destfile = tempr_nhtr_filepath, mode = "wb")
}


read_tempr_nhtr_dataset <- function() {
  col_names <- c("Year", "T", "LF", "LF-", "LF+", "A-", "A+", "AB-", "AB+")
  dataset_path <- get_amc_dataset_path(tempr_nhtr_dataset, tempr_nhtr_filename)
  readr::read_table(dataset_path, col_names = col_names, col_types = "idddddddd",  skip = 93) |>
    dplyr::mutate(
      zone_type_code = hemisphere_zone_type_code,
      zone_code = northern_hemisphere_zone_code
    ) |>
    dplyr::select(
      "zone_type_code",
      "zone_code",
      year = "Year",
      temperature_anomaly = "T"
    )
}

