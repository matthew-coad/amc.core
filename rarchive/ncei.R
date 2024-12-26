ncei_url <- "https://www.ncei.noaa.gov"

#' NCEI datasource code
#'
#' @export
#'
#' @examples
#' ncei_datasource_code
ncei_datasource_code <- "ncei"

#' National Centers for Environment Information datasource
#'
#' @export
#'
#' @examples
#' ncei_datasource
ncei_datasource <- new_amc_datasource(
  ncei_datasource_code,
  "National Centers for Environmental Information",
  ncei_url
)

#' NCEI total solar irradiance data set unique identity code
#'
#' @export
#'
#' @examples
#' ncei_total_solar_irradiance_code
ncei_total_solar_irradiance_code <- "tsi"

#' NCEI total solar irradiance meta information
#'
#' @export
#'
#' @examples
#' ncei_total_solar_irradiance_dataset
ncei_total_solar_irradiance_dataset <- new_amc_dataset(
  ncei_datasource_code,
  ncei_total_solar_irradiance_code,
  "Total solar irradiance"
)

#' List all datasets available from the NCEI data source
#'
#' @return Dataset tibble
#' @export
#'
#' @examples
#' list_issi_dataset()
list_ncei_dataset <- function() {
  ncei_total_solar_irradiance_dataset
}

ncei_total_solar_irradiance_url <- "cdr-total-solar-irradiance"
ncei_total_solar_irradiance_datafiles_filename <- "datafiles.rda"

get_ncei_total_solar_irradiance_catalog <- function() {
  catalog_uri <- paste(ncei_url, "thredds", "catalog", ncei_total_solar_irradiance_url, "catalog.xml", sep = "/")
  thredds::CatalogNode$new(catalog_uri, prefix = "thredds")
}

#' Download NCEI total solar irradiance datafiles to its local repository
#'
#' @returns Filepath of the downloaded files
#' @export
#'
#' @examples
#' download_ncei_total_solar_irradiance()
download_ncei_total_solar_irradiance <- function() {
  message(paste("[+] downloading", ncei_datasource$datasource_name, ncei_total_solar_irradiance_dataset$dataset_name))
  prepare_amc_dataset_repository(ncei_total_solar_irradiance_dataset)
  message("[i] downloading catalog")
  catalog <- get_ncei_total_solar_irradiance_catalog()
  http_service <- catalog$list_services()[['HTTPServer']]
  http_service_url <- paste0(ncei_url, http_service[["base"]])
  daily_catalog <- catalog$get_catalogs("daily")[["daily"]]
  datasets <- daily_catalog$list_datasets()
  datafiles_filename <- get_amc_dataset_path(ncei_total_solar_irradiance_dataset, ncei_total_solar_irradiance_datafiles_filename)
  saveRDS(datasets, file = datafiles_filename )

  dataset_count <- length(datasets)
  dataset_index <- 1
  message(paste("[i] downloading", dataset_count, "data files"))
  files <- c()
  for (dataset in datasets) {
    urlpath <- dataset[['urlPath']]
    filename <- dataset[['name']]
    url <- paste0(http_service_url, urlpath)
    message(paste0("[i] downloading ", filename, " (", dataset_index, " of ", dataset_count, ")"))
    destfile <- get_amc_dataset_path(ncei_total_solar_irradiance_dataset, filename)
    utils::download.file(url = url, destfile = destfile, mode = "wb")
    files[dataset_index] <- normalizePath(destfile)
    dataset_index <- dataset_index + 1
  }
  files
}

list_ncei_total_solar_irradiance_datafiles <- function() {
  datafiles <- readRDS(get_amc_dataset_path(ncei_total_solar_irradiance_dataset, ncei_total_solar_irradiance_datafiles_filename))
  filenames <- unname(unlist(purrr::map(datafiles, "name")))
  filename_parts <- strsplit(filenames, split="_")
  filename_filter <- function(filename) {
    filename_parts <- strsplit(filename, split="_")
    filename_parts[[1]][2] == "v03r00" & endsWith(filename_parts[[1]][4], "0101")
  }
  filenames |> purrr::keep(filename_filter)
}

read_ncei_total_solar_irradiance_datafile <- function(datafile) {
  tsi_nc <- tidync::tidync(get_amc_dataset_path(ncei_total_solar_irradiance_dataset, datafile)) |>
    tidync::activate("D0")
  tsi_tibble <- tsi_nc |> tidync::hyper_tibble()
  tsi_tibble |>
    dplyr::mutate(
      zone_type_code = global_zone_type_code,
      zone_code = global_zone_code,
      date = lubridate::ymd(.data$time)
    ) |>
    dplyr::select(
      .data$zone_type_code,
      .data$zone_code,
      date,
      solar_irradiance = .data$TSI,
      solar_irradiance_uncertainty = .data$TSI_UNC
    )
}

read_ncei_total_solar_irradiance <- function() {
  datafiles <- list_ncei_total_solar_irradiance_datafiles()
  dplyr::bind_rows(purrr::map(datafiles, read_ncei_total_solar_irradiance_datafile))
}

