#' Met Office United Kingdom data source
#'
#' @export
#'
#' @examples
#' metuk
metuk <- new_amc_datasource(
  "metuk",
  "Met Office United Kingdom",
  reference_url = "https://www.metoffice.gov.uk/"
)

#' Met Office Hadley Centre Central England Temperature, Mean HadCET Data, Monthly Mean data set
#'
#' @export
#'
#' @examples
#' metuk_hadcet_monthly
metuk_hadcet_monthly <- new_amc_dataset(
  metuk,
  "metuk_hadcet_monthly",
  "METUK, Surface Temperature, Central England, Monthly, Years 1659-Present",
  "Met Office Hadley Centre Central England Temperature, Mean HadCET Data, Monthly Mean",
  reference_url = "https://www.metoffice.gov.uk/hadobs/hadcet/"
)

metuk_hadcet_monthly_mean_filename <- "meantemp_monthly_totals.txt"
metuk_hadcet_monthly_mean_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt"

metuk_hadcet_monthly_min_filename <- "meantemp_monthly_totals.txt"
metuk_hadcet_monthly_min_url <- "https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt"

#' Download met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Datasets
download_metuk_hadcet_monthly <- function() {
  metuk_hadcet_monthly_min_path <- get_amc_dataset_path(metuk_hadcet_monthly, metuk_hadcet_monthly_min_filename)
  utils::download.file(url = metuk_hadcet_monthly_min_url, destfile = metuk_hadcet_monthly_min_path, mode = "wb")
}

metuk_hadcet_monthly$download <- download_metuk_hadcet_monthly

#' Read Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Monthly Dataset
read_metuk_hadcet_monthly <- function() {
  month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  col_names <- c("Year", month_names, "Annual")
  col_types <- "iddddddddddddd"
  metuk_hadcet_monthly_min_path <- get_amc_dataset_path(metuk_hadcet_monthly, metuk_hadcet_monthly_min_filename)
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

metuk_hadcet_monthly$read <- read_metuk_hadcet_monthly

#' Met Office Hadley Centre Central England Temperature, Mean HadCET Data, Yearly Mean data set
#'
#' @export
#'
#' @examples
#' metuk_hadcet_yearly
metuk_hadcet_yearly <- new_amc_dataset(
  metuk,
  "metuk_hadcet_yearly",
  "METUK, Surface Temperature, Central England, Yearly, Years 1659-Present",
  "Met Office Hadley Centre Central England Temperature, Mean HadCET Data, Monthly Mean",
  reference_url = "https://www.metoffice.gov.uk/hadobs/hadcet/"
)


#' Download met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Yearly Datasets
download_metuk_hadcet_yearly <- function() {
  # We use the monthly dataset, so nothing to do
}

metuk_hadcet_yearly$download <- download_metuk_hadcet_yearly

#' Read Met Office United Kingdom, Hadley Centre Central England Temperature (HadCET) Yearly Dataset
read_metuk_hadcet_yearly <- function() {
  month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  col_names <- c("Year", month_names, "Annual")
  col_types <- "iddddddddddddd"
  metuk_hadcet_monthly_min_path <- get_amc_dataset_path(metuk_hadcet_monthly, metuk_hadcet_monthly_min_filename)
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

metuk_hadcet_yearly$read <- read_metuk_hadcet_yearly
