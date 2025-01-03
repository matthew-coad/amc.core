#' International Space Science Institute (ISSI) datasource
#'
#' @export
#'
#' @examples
#' issi
issi <- new_amc_datasource(
  "issi",
  "International Space Science Institute (ISSI)",
  reference_code = "ISSI",
  reference_url = "https://www.issibern.ch/"
)

#' ISSI Total Solar Irradiance Composite data set
#'
#' @export
#'
#' @examples
#' issi_tsicomp
issi_tsicomp <- new_amc_dataset(
  issi,
  "issi_tsicomp",
  "ISSI, Total Solar Irradiance, Global, Daily, Years 1978-2015",
  "Total Solar Irradiance Composite",
  reference_url = "https://www.issibern.ch/teams/solarirradiance/Publications.html"
)

issi_tsicomp_url <- "https://www.issibern.ch/teams/solarirradiance/TSI_composite_DeWit.txt"
issi_tsicomp_filename <- "TSI_composite_DeWit.txt"

download_issi_tsicomp <- function() {
  issi_tsicomp_filepath <- get_amc_dataset_path(issi_tsicomp, issi_tsicomp_filename)
  utils::download.file(url = issi_tsicomp_url, destfile = issi_tsicomp_filepath, mode = "wb")
}

issi_tsicomp$download <- download_issi_tsicomp

read_issi_tsicomp <- function() {
  col_names <- c("year", "month", "day", "hour", "julian_date", "TSIo", "dTSIo", "TSIc", "dTSIc", "nobs")
  issi_tsicomp_filepath <- get_amc_dataset_path(issi_tsicomp, issi_tsicomp_filename)
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

issi_tsicomp$read <- read_issi_tsicomp
