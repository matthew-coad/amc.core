#' International Space Science Institute (ISSI) datasource
#'
#' @export
#'
#' @examples
#' issi_datasource
issi_datasource <- new_amc_datasource(
  "issi",
  "International Space Science Institute (ISSI)",
  "https://www.issibern.ch/"
)

#' ISSI Total Solar Irradiance Composite data set
#'
#' @export
#'
#' @examples
#' issi_tsicomp_dataset
issi_tsicomp_dataset <- new_amc_dataset(
  issi_datasource,
  "issi_tsicomp",
  "ISSI Total Solar Irradiance Composite"
)

issi_tsicomp_reference_url <- "https://www.issibern.ch/teams/solarirradiance/Publications.html"
issi_tsicomp_url <- "https://www.issibern.ch/teams/solarirradiance/TSI_composite_DeWit.txt"
issi_tsicomp_filename <- "TSI_composite_DeWit.txt"

download_issi_tsicomp_dataset <- function() {
  issi_tsicomp_filepath <- get_amc_dataset_path(issi_tsicomp_dataset, issi_tsicomp_filename)
  utils::download.file(url = issi_tsicomp_url, destfile = issi_tsicomp_filepath, mode = "wb")
}

read_issi_tsicomp_dataset <- function() {
  col_names <- c("year", "month", "day", "hour", "julian_date", "TSIo", "dTSIo", "TSIc", "dTSIc", "nobs")
  issi_tsicomp_filepath <- get_amc_dataset_path(issi_tsicomp_dataset, issi_tsicomp_filename)
  readr::read_table(issi_tsicomp_filepath, col_names = col_names, col_types = "iiiiiddddi_",  comment = ";") |>
    dplyr::mutate(
      date= lubridate::ymd(paste0(.data$year, "-", .data$month, "-", .data$day))
    ) |>
    dplyr::select(
      "date",
      solar_irradiance = "TSIc",
      solar_irradiance_uncertainty = "dTSIc"
    )
}

