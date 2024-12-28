#' Class that indicates an object is an amc dataset
#'
#' @export
#'
#' @examples
#' amc_dataset_class %in% class(giss_ta_zonal_yearly_dataset)
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
#' example_datasource <- new_amc_datasource(
#'   "example",
#'   "AMC Example datasource",
#'   "https://example.com/"
#' )
#' example_1_dataset <- new_amc_dataset(
#'   example_datasource,
#'   "example_1",
#'   "AMC Example Dataset 1"
#' )
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
#' @param envir Where to look for datasets objects. Defaults to parent frame.
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \donttest{
#'   list_amc_dataset()
#' }
list_amc_dataset <- function(envir = parent.frame()) {
  packages <- base::search() |> purrr::discard(\(x) x == ".GlobalEnv" | x == "Autoloads")
  envir_object_names = ls(envir = envir)
  package_object_names <- packages |>
    purrr::map(\(x) ls(x)) |>
    unlist(recursive = TRUE)
  object_names <- c(envir_object_names, package_object_names) |> unique()
  amc_datasets <- object_names |>
    purrr::map(\(x) get(x, envir=envir)) |>
    purrr::keep(\(x) amc_dataset_class %in% class(x))
  dplyr::bind_rows(amc_datasets)
}

#' Resolve value that identifies a dataset
#'
#' @param x Dataset identifier. Possible values include the datasets code or the dataset itself
#' @param envir Where to look for datasets objects. Defaults to parent frame.
#'
#' @returns AMC dataset
resolve_amc_dataset <- function(x, envir = parent.frame()) {
  if (amc_dataset_class %in% class(x)) {
    return(x)
  }
  else if (is.character(x)) {
    list_amc_dataset(envir = envir) |> dplyr::filter(.data$dataset_code == x)
  }
  else {
    stop("x unexpected type")
  }
}

#' Non-destructively prepares data set repository for use
#'
#' @param dataset Dataset to prepare repository for
#' @param envir Where to look for datasets objects. Defaults to parent frame.
#'
#' @return Respository path
#' @export
#'
#' @examples
#' \donttest{
#'  prepare_amc_dataset_repository(giss_ta_zonal_yearly_dataset)
#' }
prepare_amc_dataset_repository <- function(dataset, envir = parent.frame()) {
  dataset <- resolve_amc_dataset(dataset, envir = envir)
  repository_path <- get_amc_dataset_repository_path(dataset)
  if (!file.exists(repository_path)){
    dir.create(repository_path, recursive=TRUE)
  }
  return(repository_path)
}

#' Import datafile into an amc datasets repository
#'
#' @param dataset Data to import file into
#' @param filepath Path to the file to import
#' @param envir Where to look for datasets objects. Defaults to parent frame.
#'
#' @return Import successfuly
#' @export
#'
#' @examples
#' \donttest{
#'   import_amc_dataset_file(test_dataset1, "ZonAnn.Ts+dSST.csv")
#' }
import_amc_dataset_file <- function(dataset, filepath, envir = parent.frame()) {
  dataset <- resolve_amc_dataset(dataset, envir = envir)
  repository_path <- prepare_amc_dataset_repository(dataset)
  file.copy(filepath, repository_path, overwrite=TRUE)
}

#' Download an amc dataset
#'
#' @param dataset Dataset to download
#' @param envir Where to look for datasets objects. Defaults to parent frame.
#'
#' @returns Updated dataset status
#' @export
#'
#' @examples
#' \donttest{
#'   download_amc_dataset(giss_ta_zonal_yearly_dataset)
#' }
download_amc_dataset <- function(dataset, envir = parent.frame()) {
  dataset <- resolve_amc_dataset(dataset, envir = envir)

  func_name <- paste0("download_", dataset$dataset_code, "_dataset")
  func <- tryCatch(get(func_name, envir = envir), error = function(cond) NULL)
  if (is.null(func)) {
    stop("dataset downloader ", func_name, " not found")
  }

  repository_path <- prepare_amc_dataset_repository(dataset)
  save_amc_dataset_state(dataset, amc_dataset_status$downloading)

  download_cond <- NULL
  tryCatch(
    func(),
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


#' Prepares a data set for use, including creating directories and downloading the data from
#' its source if needed.
#'
#' By default prepare is non-destructive and will leave existing resource in place
#'
#' @param dataset Dataset to prepare
#' @param envir Where to look for datasets objects. Defaults to parent frame.
#'
#' @return Dataset state
#' @export
#'
#' @examples
#' \donttest{
#'   prepare_amc_dataset(giss_ta_zonal_yearly_dataset)
#' }
prepare_amc_dataset <- function(dataset, envir = parent.frame()) {
  dataset <- resolve_amc_dataset(dataset, envir = envir)
  prepare_amc_dataset_repository(dataset)
  state <- read_amc_dataset_state(dataset)
  if (state$status != amc_dataset_status$ready) {
    state <- download_amc_dataset(dataset, envir = envir)
  }
  return(state)
}

#' Read amc dataset data
#'
#' @param dataset Dataset to read
#' @param envir Where to look for datasets objects. Defaults to parent frame.
#'
#' @returns Datasets data as AMC validated tibble
#' @export
#'
#' @examples
#' \donttest{
#'   read_amc_dataset(test_dataset1)
#' }
read_amc_dataset <- function(dataset, envir = parent.frame()) {
  dataset <- resolve_amc_dataset(dataset, envir = envir)
  state <- prepare_amc_dataset(dataset)
  if (state$status != amc_dataset_status$ready) {
    stop("Dataset ", dataset$dataset_name, " is not ready")
  }
  func_name <- paste0("read_", dataset$dataset_code, "_dataset")
  func <- tryCatch(match.fun(func_name), error = function(cond) NULL)
  if (is.null(func)) {
    stop("dataset reader ", func_name, " not found")
  }
  func()
}
