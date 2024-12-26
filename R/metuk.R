#' Met Office United Kingdom datasource
#'
#' @export
#'
#' @examples
#' metuk_datasource
metuk_datasource <- new_amc_datasource(
  "metuk",
  "Met Office United Kingdom",
  "https://www.metoffice.gov.uk/"
)

#' Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Monthly Dataset
#'
#' @export
#'
#' @examples
#' metuk_hadcet_monthly_dataset
metuk_hadcet_monthly_dataset <- new_amc_dataset(
  metuk_datasource,
  "metuk_hadcet_monthly",
  "Hadley Centre Central England Temperature (HadCET) monthly dataset"
)

#' Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Daily Dataset
#'
#' @export
#'
#' @examples
#' metuk_hadcet_daily_dataset
metuk_hadcet_daily_dataset <- new_amc_dataset(
  metuk_datasource,
  "metuk_hadcet_daily",
  "Hadley Centre Central England Temperature (HadCET) Daily dataset"
)

metuk_hadcet_daily_mean_filename <- "meantemp_daily_totals.txt"
metuk_hadcet_daily_mean_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_daily_totals.txt"

metuk_hadcet_daily_min_filename <- "mintemp_daily_totals.txt"
metuk_hadcet_daily_min_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/mintemp_daily_totals.txt"

metuk_hadcet_daily_max_filename <- "maxtemp_daily_totals.txt"
metuk_hadcet_daily_max_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/maxtemp_daily_totals.txt"

#' Download Hadley Centre Central England Temperature (HadCET) Daily Dataset
#'
#' @returns Dataset state
#' @export
#'
#' @examples
#' download_metuk_hadcet_daily_dataset()
download_metuk_hadcet_daily_dataset <- function() {
  run_download_amc_dataset(metuk_hadcet_daily_dataset, {
    metuk_hadcet_daily_mean_path <- get_amc_dataset_path(metuk_hadcet_daily_dataset, metuk_hadcet_daily_mean_filename)
    utils::download.file(url = metuk_hadcet_daily_mean_url, destfile = metuk_hadcet_daily_mean_path, mode = "wb")

    metuk_hadcet_daily_min_path <- get_amc_dataset_path(metuk_hadcet_daily_dataset, metuk_hadcet_daily_min_filename)
    utils::download.file(url = metuk_hadcet_daily_min_url, destfile = metuk_hadcet_daily_min_path, mode = "wb")

    metuk_hadcet_daily_max_path <- get_amc_dataset_path(metuk_hadcet_daily_dataset, metuk_hadcet_daily_max_filename)
    utils::download.file(url = metuk_hadcet_daily_max_url, destfile = metuk_hadcet_daily_max_path, mode = "wb")
  })
}

metuk_hadcet_monthly_mean_filename <- "meantemp_monthly_totals.txt"
metuk_hadcet_monthly_mean_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt"

metuk_hadcet_monthly_min_filename <- "meantemp_monthly_totals.txt"
metuk_hadcet_monthly_min_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt"

#' Download met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Datasets
#'
#' @returns Dataset state
#' @export
#'
#' @examples
#' download_metuk_hadcet_monthly_dataset()
download_metuk_hadcet_monthly_dataset <- function() {
  run_download_amc_dataset(metuk_hadcet_monthly_dataset, {
    metuk_hadcet_monthly_min_path <- get_amc_dataset_path(metuk_hadcet_monthly_dataset, metuk_hadcet_monthly_min_filename)
    utils::download.file(url = metuk_hadcet_monthly_min_url, destfile = metuk_hadcet_monthly_min_path, mode = "wb")
  })
}

#' Read Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Monthly Dataset
#'
#' @return Tibble
#' @export
#'
#' @examples
#' read_metuk_hadcet_monthly()
read_metuk_hadcet_monthly <- function() {
  prepare_amc_dataset(metuk_hadcet_monthly_dataset)
  month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  col_names <- c("Year", month_names, "Annual")
  col_types <- "iddddddddddddd"
  metuk_hadcet_monthly_min_path <- get_amc_dataset_path(metuk_hadcet_monthly_dataset, metuk_hadcet_monthly_min_filename)
  readr::read_table(metuk_hadcet_monthly_min_path, col_names = col_names, col_types = col_types, skip = 5) |>
    tidyr::pivot_longer(!"Year", names_to = "Month", values_to = "mean_temperature" ) |>
    dplyr::filter(.data$Month != "Annual") |>
    dplyr::mutate(
      year = .data$Year,
      month = which(month_names == .data$Month),
    ) |>
    dplyr::select(
      "year",
      "month",
      "mean_temperature"
    )
}

#' Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Year Dataset
#'
#' @export
#'
#' @examples
#' metuk_hadcet_monthly_dataset
metuk_hadcet_yearly_dataset <- new_amc_dataset(
  metuk_datasource,
  "metuk_hadcet_yearly",
  "Hadley Centre Central England Temperature (HadCET) yearly dataset"
)


#' Download met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Yearly Datasets
#'
#' @returns Dataset state
#' @export
#'
#' @examples
#' download_metuk_hadcet_monthly_dataset()
download_metuk_hadcet_yearly_dataset <- function() {
  run_download_amc_dataset(metuk_hadcet_monthly_dataset, {
    # We use the monthly dataset, so nothing to do
  })
}

#' Read Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Yearly Dataset
#'
#' @return Tibble
#' @export
#'
#' @examples
#' read_metuk_hadcet_monthly()
read_metuk_hadcet_yearly <- function() {
  # We use the monthly dataset, so prepare it instead
  prepare_amc_dataset(metuk_hadcet_monthly_dataset)
  month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  col_names <- c("Year", month_names, "Annual")
  col_types <- "iddddddddddddd"
  metuk_hadcet_monthly_min_path <- get_amc_dataset_path(metuk_hadcet_monthly_dataset, metuk_hadcet_monthly_min_filename)
  readr::read_table(metuk_hadcet_monthly_min_path, col_names = col_names, col_types = col_types, skip = 5) |>
    dplyr::mutate(
      year = .data$Year,
      mean_temperature = .data$Annual
    ) |>
    dplyr::select(
      "year",
      "mean_temperature"
    )
}
