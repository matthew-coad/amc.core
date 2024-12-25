
#' Met Office United Kingdom datasource code
#'
#' @export
#'
#' @examples
#' metuk_datasource_code
metuk_datasource_code <- "metuk"

#' Met Office United Kingdom datasource
#'
#' @export
#'
#' @examples
#' iac_datasource
metuk_datasource <- new_amc_datasource(
  metuk_datasource_code,
  "Met Office United Kingdom",
  "https://www.metoffice.gov.uk/"
)

#' Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Monthly Dataset Code
#'
#' @export
#'
#' @examples
#' metuk_hadcet_monthly_dataset_code
metuk_hadcet_monthly_dataset_code <- "metuk_hadcet_monthly"

#' Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Monthly Dataset
#'
#' @export
#'
#' @examples
#' metuk_hadcet_monthly_dataset
metuk_hadcet_monthly_dataset <- new_amc_dataset(
  metuk_datasource_code,
  metuk_hadcet_monthly_dataset_code,
  "Hadley Centre Central England Temperature (HadCET) monthly dataset"
)

#' Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Daily Dataset Code
#'
#' @export
#'
#' @examples
#' metuk_hadcet_daily_dataset_code
metuk_hadcet_daily_dataset_code <- "metuk_hadcet_daily"

#' Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Daily Dataset
#'
#' @export
#'
#' @examples
#' metuk_hadcet_daily_dataset
metuk_hadcet_daily_dataset <- new_amc_dataset(
  metuk_datasource_code,
  metuk_hadcet_daily_dataset_code,
  "Hadley Centre Central England Temperature (HadCET) Daily dataset"
)


metuk_hadcet_daily_mean_filename <- "meantemp_daily_totals.txt"
metuk_hadcet_daily_mean_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_daily_totals.txt"

metuk_hadcet_daily_min_filename <- "mintemp_daily_totals.txt"
metuk_hadcet_daily_min_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/mintemp_daily_totals.txt"

metuk_hadcet_daily_max_filename <- "maxtemp_daily_totals.txt"
metuk_hadcet_daily_max_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/maxtemp_daily_totals.txt"

metuk_hadcet_monthly_mean_filename <- "meantemp_monthly_totals.txt"
metuk_hadcet_monthly_mean_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt"

metuk_hadcet_monthly_min_filename <- "meantemp_monthly_totals.txt"
metuk_hadcet_monthly_min_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt"

download_metuk_hadcet_daily <- function() {
  prepare_amc_dataset_repository(metuk_hadcet_daily_dataset)
  files <- c()

  metuk_hadcet_daily_mean_path <- get_amc_dataset_path(metuk_hadcet_daily_dataset, metuk_hadcet_daily_mean_filename)
  utils::download.file(url = metuk_hadcet_daily_mean_url, destfile = metuk_hadcet_daily_mean_path, mode = "wb")
  files[1] <- normalizePath(metuk_hadcet_daily_mean_path)

  metuk_hadcet_daily_min_path <- get_amc_dataset_path(metuk_hadcet_daily_dataset, metuk_hadcet_daily_min_filename)
  utils::download.file(url = metuk_hadcet_daily_min_url, destfile = metuk_hadcet_daily_min_path, mode = "wb")
  files[2] <- normalizePath(metuk_hadcet_daily_min_path)

  metuk_hadcet_daily_max_path <- get_amc_dataset_path(metuk_hadcet_daily_dataset, metuk_hadcet_daily_max_filename)
  utils::download.file(url = metuk_hadcet_daily_max_url, destfile = metuk_hadcet_daily_max_path, mode = "wb")
  files[3] <- normalizePath(metuk_hadcet_daily_max_path)

  files
}

#' Download
#'
#' @returns Vector of downloaded files
#' @export
#'
#' @examples
#' download_metuk_hadcet_monthly()
download_metuk_hadcet_monthly <- function() {
  prepare_amc_dataset_repository(metuk_hadcet_monthly_dataset)
  files <- c()

  metuk_hadcet_monthly_min_path <- get_amc_dataset_path(metuk_hadcet_monthly_dataset, metuk_hadcet_monthly_min_filename)
  utils::download.file(url = metuk_hadcet_monthly_min_url, destfile = metuk_hadcet_monthly_min_path, mode = "wb")
  files[1] <- normalizePath(metuk_hadcet_monthly_min_path)

  files
}

#' Read Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Monthly Dataset
#'
#' @return Tibble
#' @export
#'
#' @examples
#' read_metuk_hadcet_monthly()
read_metuk_hadcet_monthly <- function() {
  col_names <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Annual")
  col_types <- "iddddddddddddd"
  metuk_hadcet_monthly_min_path <- get_amc_dataset_path(metuk_hadcet_monthly_dataset, metuk_hadcet_monthly_min_filename)
  readr::read_table(metuk_hadcet_monthly_min_path, col_names = col_names, col_types = col_types, skip = 5) |>
    tidyr::pivot_longer(!Year, names_to = "Month", values_to = "mean_temperature" ) |>
    dplyr::filter(Month != "Annual") |>
    dplyr::mutate(
      dataset_code = metuk_hadcet_monthly_dataset_code,
      date = lubridate::ymd(paste0(.data$Year, "-", .data$Month, "-1"))
    ) |>
    dplyr::select(
      .data$dataset_code,
      .data$date,
      .data$mean_temperature
    )
}


