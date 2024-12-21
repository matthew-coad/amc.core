
#' International Space Science Institute data set code
#'
#' @export
#'
#' @examples
#' issi_datasource_code
issi_datasource_code <- "issi"

#' International Space Science Institute data set
#'
#' @export
#'
#' @examples
#' issi_datasource
issi_datasource <- new_amc_datasource(
  issi_datasource_code,
  "International Space Science Institute",
  "https://www.issibern.ch/"
)

#' Total Solar Irradiance Composite data set code
#'
#' @export
#'
#' @examples
#' issi_tsi_comp_dataset_code
issi_tsi_comp_dataset_code <- "tsi_comp"

#' Total Solar Irradiance Composite data set
#'
#' @export
#'
#' @examples
#' issi_tsi_comp_dataset
issi_tsi_comp_dataset <- new_amc_dataset(
  issi_datasource_code,
  issi_tsi_comp_dataset_code,
  "Total Solar Irradiance Composite"
)

#' List all datasets available from the International Space Science Institute data set
#'
#' @return Dataset tibble
#' @export
#'
#' @examples
#' list_issi_dataset()
list_issi_dataset <- function() {
  issi_tsi_comp_dataset
}

issi_tsi_comp_file_name <- "TSI_composite_DeWit.txt"

#' Read Total Solar Irradiance Composite dataset
#'
#' @return Tibble
#' @export
#'
#'
#' @examples
#' read_issi_tsi_comp()
read_issi_tsi_comp <- function() {
  col_names <- c("year", "month", "day", "hour", "julian_date", "TSIo", "dTSIo", "TSIc", "dTSIc", "nobs")
  dataset_path <- get_amc_dataset_path(issi_tsi_comp_dataset, issi_tsi_comp_file_name)
  readr::read_table(dataset_path, col_names = col_names, col_types = "iiiiiddddi_",  comment = ";") |>
    dplyr::mutate(
      datasource_code = issi_datasource_code,
      dataset_code = issi_tsi_comp_dataset_code,
      zone_type_code = global_zone_type_code,
      zone_code = global_zone_code,
      date= lubridate::ymd(paste0(.data$year, "-", .data$month, "-", .data$day))
    ) |>
    dplyr::select(
      .data$datasource_code,
      .data$dataset_code,
      .data$zone_type_code,
      .data$zone_code,
      .data$date,
      solar_irradiance = .data$TSIc,
      solar_irradiance_uncertainty = .data$dTSIc
    )
}

