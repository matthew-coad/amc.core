#' Class that indicates an object is an amc dataset
#'
#' @export
#'
#' @examples
#' amc_dataset_class %in% class(test_dataset1)
amc_dataset_class <- "amc_dataset"

#' Create new amc dataset
#'
#' @param datasource Datasource that owns the dataset
#' @param dataset_code Unique dataset code
#' @param dataset_name Data set name
#'
#' @returns Dataset tibble
#' @export
#'
#' @examples
#' new_amc_dataset(test_datasource, "test_dataset1", "Test dataset1")
new_amc_dataset <- function(datasource, dataset_code, dataset_name) {
  dataset <- tibble::tibble(
    datasource_code = datasource$datasource_code,
    dataset_code,
    dataset_name
  )
  class(dataset) <- c(amc_dataset_class, class(dataset))
  dataset
}

#' List all available amc datasets
#'
#' @return Tibble
#' @export
#'
#' @examples
#' list_amc_dataset()
list_amc_dataset <- function() {
  packages <- base::search() |> purrr::discard(\(x) x == ".GlobalEnv" | x == "Autoloads")
  object_names <- packages |>
    purrr::map(ls) |>
    unlist(recursive = TRUE) |>
    unique()
  amc_datasource <- object_names |>
    purrr::map(get) |>
    purrr::keep(\(x)amc_dataset_class %in% class(x))
  dplyr::bind_rows(amc_datasource)
}

#' Non-destructively prepares data set repository for use
#'
#' @param dataset Dataset to prepare repository for
#'
#' @return Respository path
#' @export
#'
#' @examples
#' prepare_amc_dataset_repository(test_dataset1)
prepare_amc_dataset_repository <- function(dataset) {
  repository_path <- get_amc_dataset_repository_path(dataset)
  if (!file.exists(repository_path)){
    dir.create(repository_path, recursive=TRUE)
  }
  return(repository_path)
}

#' Non-destructively prepares a data set for use
#'
#' @param dataset Dataset to prepare
#'
#' @return Dataset state
#' @export
#'
#' @examples
#' prepare_amc_dataset(test_dataset1)
prepare_amc_dataset <- function(dataset) {
  prepare_amc_dataset_repository(dataset)
  state <- read_amc_dataset_state(dataset)
  if (state$status != amc_dataset_status$ready) {
    state <- download_amc_dataset(dataset)
  }
  return(state)
}

#' Import datafile into an amc datasets repository
#'
#' @param dataset Data to import file into
#' @param filepath Path to the file to import
#'
#' @return Import successfuly
#' @export
#'
#' @examples
#' import_amc_dataset_file(test_dataset1, "ZonAnn.Ts+dSST.csv")
import_amc_dataset_file <- function(dataset, filepath) {
  repository_path <- prepare_amc_dataset_repository(dataset)
  file.copy(filepath, repository_path, overwrite=TRUE)
}

#' Get underlying download dataset implementation
#'
#' @param dataset Target Dataset
#' @param required If true function is required and will fail if it is missing
#'
#' @returns Function that takes no parameters, NULL if function cannot be found
get_download_amc_dataset_func <- function(dataset, required = FALSE) {
  func_name <- paste0("download_", dataset$dataset_code, "_dataset")
  func <- tryCatch(match.fun(func_name), error = function(cond) NULL)
  if (required && is.null(func)) {
    stop("dataset downloader ", func_name, " not found")
  }
  return(func)
}

#' Download amc dataset
#'
#' @param dataset Dataset to download
#'
#' @returns Updated dataset status
#' @export
#'
#' @examples
#' download_amc_dataset(test_dataset1)
download_amc_dataset <- function(dataset) {
  get_download_amc_dataset_func(dataset, required = TRUE)()
}

#' Run download of an amc dataset.
#'
#' @param dataset The dataset to download
#' @param downloader The script block that performs customised download
#' @param envir The environment in which expr is to be evaluated.
#'
#'
#' @returns Updated dataset status
#' @export
#'
#' @examples
#' run_download_amc_dataset(test_dataset1, {
#'   test_datafile1_filepath <- get_amc_dataset_path(test_dataset1, test_datafile1_filename)
#'   fileConn<-file(test_datafile1_filepath)
#'   data_lines <- c(
#'     "test_dimension1,test_measure1,test_measure2",
#'     "Dimension-1-1,11,12",
#'     "Dimension-2-1,21,22"
#'   )
#'   writeLines(data_lines, fileConn)
#'   close(fileConn)
#' })
run_download_amc_dataset <- function(dataset, downloader, envir = parent.frame()) {
  prepare_amc_dataset_repository(dataset)
  save_amc_dataset_state(dataset, amc_dataset_status$downloading)
  download_cond <- NULL
  tryCatch(
    eval(downloader, envir = envir),
    error = function(cond) {
      download_cond <<- cond
      message(conditionMessage(cond))
    }
  )
  if (is.null(download_cond)) {
    return (save_amc_dataset_state(dataset, amc_dataset_status$ready, lubridate::now()))
  } else {
    return (save_amc_dataset_state(dataset, amc_dataset_status$download_failed, fail_condition = download_cond))
  }
}

#' Get underlying read dataset implementation
#'
#' @param dataset Target Dataset
#' @param required If true function is required and will fail if it is missing
#'
#' @returns Function that takes no parameters, NULL if function cannot be found
get_read_amc_dataset_func <- function(dataset, required = FALSE) {
  func_name <- paste0("read_", dataset$dataset_code)
  func <- tryCatch(match.fun(func_name), error = function(cond) NULL)
  if (required && is.null(func)) {
    stop("dataset reader ", func_name, " not found")
  }
  return(func)
}

#' Read amc dataset data
#'
#' @param dataset Dataset to read
#'
#' @returns Datasets data as AMC validated tibble
#' @export
#'
#' @examples
#' read_amc_dataset(test_dataset1)
read_amc_dataset <- function(dataset) {
  get_read_amc_dataset_func(dataset, required = TRUE)()
}
