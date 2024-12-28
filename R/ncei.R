ncei_url <- "https://www.ncei.noaa.gov"

#' National Centers for Environment Information datasource
#'
#' @export
#'
#' @examples
#' ncei_datasource
ncei_datasource <- new_amc_datasource(
  "ncei",
  "National Centers for Environmental Information",
  ncei_url
)

#' NCEI total solar irradiance Dataset
#'
#' @export
#'
#' @examples
#' ncei_tsi_dataset
ncei_tsi_dataset <- new_amc_dataset(
  ncei_datasource,
  "ncei_tsi",
  "NCEI Total solar irradiance"
)

ncei_tsi_url <- "cdr-total-solar-irradiance"
ncei_tsi_datafiles_filename <- "datafiles.rda"
ncei_tsi_filename <- "ncei_tsi.rda"

get_ncei_tsi_catalog <- function() {
  catalog_uri <- paste(ncei_url, "thredds", "catalog", ncei_tsi_url, "catalog.xml", sep = "/")
  thredds::CatalogNode$new(catalog_uri, prefix = "thredds")
}

#' Download NCEI total solar irradiance datafiles to its local repository
download_ncei_tsi_dataset <- function() {
  message(paste("[+] downloading", ncei_datasource$datasource_name, ncei_tsi_dataset$dataset_name))
  message("[i] downloading catalog")
  catalog <- get_ncei_tsi_catalog()
  http_service <- catalog$list_services()[['HTTPServer']]
  http_service_url <- paste0(ncei_url, http_service[["base"]])
  daily_catalog <- catalog$get_catalogs("daily")[["daily"]]
  datasets <- daily_catalog$list_datasets()
  datafiles_filename <- get_amc_dataset_path(ncei_tsi_dataset, ncei_tsi_datafiles_filename)
  saveRDS(datasets, file = datafiles_filename )

  dataset_count <- length(datasets)
  dataset_index <- 1
  message(paste("[i] downloading", dataset_count, "data files"))
  for (dataset in datasets) {
    urlpath <- dataset[['urlPath']]
    filename <- dataset[['name']]
    url <- paste0(http_service_url, urlpath)
    message(paste0("[i] downloading ", filename, " (", dataset_index, " of ", dataset_count, ")"))
    destfile <- get_amc_dataset_path(ncei_tsi_dataset, filename)
    utils::download.file(url = url, destfile = destfile, mode = "wb")
    dataset_index <- dataset_index + 1
  }

  message("[i] Preprocessing datafiles")
  datafiles <- list_ncei_tsi_datafiles()
  converted_data  <- dplyr::bind_rows(purrr::map(datafiles, read_ncei_tsi_datafile))
  saveRDS(converted_data, file = get_amc_dataset_path(ncei_tsi_dataset, ncei_tsi_filename))
}

list_ncei_tsi_datafiles <- function() {
  datafiles <- readRDS(get_amc_dataset_path(ncei_tsi_dataset, ncei_tsi_datafiles_filename))
  filenames <- unname(unlist(purrr::map(datafiles, "name")))
  filename_parts <- strsplit(filenames, split="_")
  filename_filter <- function(filename) {
    filename_parts <- strsplit(filename, split="_")
    filename_parts[[1]][2] == "v03r00" & endsWith(filename_parts[[1]][4], "0101")
  }
  filenames |> purrr::keep(filename_filter)
}

read_ncei_tsi_datafile <- function(datafile) {
  tsi_nc <- tidync::tidync(get_amc_dataset_path(ncei_tsi_dataset, datafile)) |>
    tidync::activate("D0")
  tsi_tibble <- tsi_nc |> tidync::hyper_tibble()
  tsi_tibble |>
    dplyr::mutate(
      date = lubridate::ymd(.data$time)
    ) |>
    dplyr::select(
      "date",
      solar_irradiance = "TSI",
      solar_irradiance_uncertainty = "TSI_UNC"
    )
}

read_ncei_tsi_dataset <- function() {
  readRDS(file = get_amc_dataset_path(ncei_tsi_dataset, ncei_tsi_filename))
}

