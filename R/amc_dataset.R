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
#' @return Tibble
#' @export
#'
#' @examples
#' \donttest{
#'   list_amc_dataset()
#' }
list_amc_dataset <- function() {
  packages <- base::search() |> purrr::discard(\(x) x == ".GlobalEnv" | x == "Autoloads")
  package_object_names <- packages |>
    purrr::map(\(x) ls(x)) |>
    unlist(recursive = TRUE)
  object_names <-package_object_names |> unique()
  amc_datasets <- object_names |>
    purrr::map(\(x) get(x)) |>
    purrr::keep(\(x) amc_dataset_class %in% class(x))
  dplyr::bind_rows(amc_datasets)
}

#' Resolve value that identifies a dataset
#'
#' @param x Dataset identifier. Possible values include the datasets code or the dataset itself
#'
#' @returns AMC dataset
resolve_amc_dataset <- function(x) {
  if (amc_dataset_class %in% class(x)) {
    return(x)
  }
  else if (is.character(x)) {
    list_amc_dataset() |> dplyr::filter(.data$dataset_code == x)
  }
  else {
    stop("x unexpected type")
  }
}

#' Non-destructively prepares data set repository for use
#'
#' @param dataset Dataset to prepare repository for
#'
#' @return Respository path
#' @export
#'
#' @examples
#' \donttest{
#'  prepare_amc_dataset_repository(giss_ta_zonal_yearly_dataset)
#' }
prepare_amc_dataset_repository <- function(dataset) {
  dataset <- resolve_amc_dataset(dataset)
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
#'
#' @return Import successfuly
#' @export
#'
#' @examples
#' \donttest{
#'   import_amc_dataset_file(test_quick_dataset, "ZonAnn.Ts+dSST.csv")
#' }
import_amc_dataset_file <- function(dataset, filepath) {
  dataset <- resolve_amc_dataset(dataset)
  repository_path <- prepare_amc_dataset_repository(dataset)
  file.copy(filepath, repository_path, overwrite=TRUE)
}

#' Download an amc dataset
#'
#' @param dataset Dataset to download
#'
#' @returns Updated dataset status
#' @export
#'
#' @examples
#' \donttest{
#'   download_amc_dataset(giss_ta_zonal_yearly_dataset)
#' }
download_amc_dataset <- function(dataset) {
  dataset <- resolve_amc_dataset(dataset)

  func_name <- paste0("download_", dataset$dataset_code, "_dataset")
  func <- tryCatch(get(func_name), error = function(cond) NULL)
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
#'
#' @return Dataset state
#' @export
#'
#' @examples
#' \donttest{
#'   prepare_amc_dataset(giss_ta_zonal_yearly_dataset)
#' }
prepare_amc_dataset <- function(dataset) {
  dataset <- resolve_amc_dataset(dataset)
  prepare_amc_dataset_repository(dataset)
  state <- read_amc_dataset_state(dataset)
  if (state$status != amc_dataset_status$ready) {
    state <- download_amc_dataset(dataset)
  }
  return(state)
}

#' Read amc dataset data
#'
#' @param dataset Dataset to read
#'
#' @returns Datasets data as AMC validated tibble
#' @export
#'
#' @examples
#' \donttest{
#'   read_amc_dataset(test_quick_dataset)
#' }
read_amc_dataset <- function(dataset) {
  dataset <- resolve_amc_dataset(dataset)
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
