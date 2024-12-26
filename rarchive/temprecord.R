
#' Temperature record data source code
#'
#' @export
#'
#' @examples
#' temprecord_datasource_code
temprecord_datasource_code <- "temprecord"

#' Temperature Record Datasource
#'
#' @export
#'
#' @examples
#' temprecord_datasource
temprecord_datasource <- new_amc_datasource(
  temprecord_datasource_code,
  "Global Historical Temperature Record",
  "https://www.temperaturerecord.org/"
)

#' 2,000-Year Northern Hemisphere Temperature Reconstruction data set code
#'
#' @export
#'
#' @examples
#' temprecord_nhtr_dataset_code
temprecord_nhtr_dataset_code <- "nhtr"

#' 2,000 Year Northern Hemisphere Temperature Anomaly Temperature Reconstruction data set
#'
#' @export
#'
#' @examples
#' temprecord_nhtr_dataset
temprecord_nhtr_dataset <- new_amc_dataset(
  temprecord_datasource_code,
  temprecord_nhtr_dataset_code,
  "Northern Hemisphere Temperature Anomaly Temperature Reconstruction, 0AD to 1880AD"
)

#' List all datasets available from the Temperature record data set
#'
#' @return Dataset tibble
#' @export
#'
#' @examples
#' list_temprecord_dataset()
list_temprecord_dataset <- function() {
  temprecord_nhtr_dataset
}

temprecord_nhtr_filename <- "nhtemp-moberg2005.txt"

#' Read 2,000-Year Northern Hemisphere Temperature Reconstruction
#'
#' @return Tibble
#' @export
#'
#'
#' @examples
#' read_temprecord_nhtr()
read_temprecord_nhtr <- function() {
  col_names <- c("Year", "T", "LF", "LF-", "LF+", "A-", "A+", "AB-", "AB+")
  dataset_path <- get_amc_dataset_path(temprecord_nhtr_dataset, temprecord_nhtr_filename)
  readr::read_table(dataset_path, col_names = col_names, col_types = "idddddddd",  comment = ";") |>
    dplyr::mutate(
      datasource_code = temprecord_datasource_code,
      dataset_code = temprecord_nhtr_dataset_code,
      zone_type_code = hemisphere_zone_type_code,
      zone_code = northern_hemisphere_zone_code
    ) |>
    dplyr::select(
      .data$datasource_code,
      .data$dataset_code,
      .data$zone_type_code,
      .data$zone_code,
      year = .data$Year,
      temperature_anomaly = .data$T
    )
}

