#' Class that indicates an object is an amc dataset
#'
#' @export
#'
#' @examples
#' amc_dataset_class %in% class(amctest_quick)
amc_dataset_class <- "amc_dataset"

#' Create new AMC dataset
#'
#' @param datasource Data source that supplies the dataset.
#' @param name Unique name of the data set. Must be a unique short alpha+numeric lower case string prefixed with data set source code
#' separated with underscore.
#' @param synopsis Brief synopsis that describes the data set, including observations it contains, zone, observation frequency and time period.
#' @param title Title of the data set. Should match name referred to in reference documentation.
#' @param reference_code Optional code that reference documentation uses to refer to the dataset
#' @param reference_url Optional url that can be used to find reference documentation for the datraset
#'
#' @returns AMC Data set list
#' @export
#'
#' @examples
#' example_datasource <- new_amc_datasource(
#'   "example",
#'   "EXAMPLE datasource",
#'   "https://example.com/"
#' )
#' example_1_dataset <- new_amc_dataset(
#'   example_datasource,
#'   "example_1",
#'   "AMCEXAMPLE, Example Dataset 1",
#'   "AMC Exmple Dataset"
#' )
new_amc_dataset <- function(datasource, name, synopsis, title, reference_code = "", reference_url = "") {
  dataset <- list(
    datasource = datasource,
    name = name,
    synopsis = synopsis,
    title = title,
    reference_code = reference_code,
    reference_url = reference_url,
    download = NULL
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
  object_names <- package_object_names |> unique()
  datasets <- object_names |>
    purrr::map(\(x) get(x)) |>
    purrr::keep(\(x) amc_dataset_class %in% class(x))
  dataset_names <- datasets |> purrr::map(\(x) x$name ) |> unlist()
  datasets <- datasets |> purrr::set_names(nm = dataset_names)
  datasets
}

#' View available AMC data sets as a dataframe
#'
#' @return Data frame containing data sets name, title, synopsis and reference title + url
#' @export
#'
#' @examples
#' \donttest{
#'   view_amc_datasource()
#' }
view_amc_dataset <- function() {
  datasources <- list_amc_dataset()
  tibble::tibble(
    name = unlist(purrr::map(datasources, "name")),
    synopsis = unlist(purrr::map(datasources, "synopsis")),
    reference_code = unlist(purrr::map(datasources, "reference_code")),
    reference_url = unlist(purrr::map(datasources, "reference_url")),
    title = unlist(purrr::map(datasources, "title"))
  )
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
    list_amc_dataset() |> purrr::keep(\(y) y$name == x) |> purrr::pluck(1)
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
#'   import_amc_dataset_file(amctest_quick, "ZonAnn.Ts+dSST.csv")
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
#'   download_amc_dataset(amctest_quick)
#' }
download_amc_dataset <- function(dataset) {
  dataset <- resolve_amc_dataset(dataset)

  func <- dataset$download
  if (is.null(func)) {
    stop("dataset downloader not set")
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
#'   prepare_amc_dataset(amctest_quick)
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
#'   read_amc_dataset(amctest_quick)
#' }
read_amc_dataset <- function(dataset) {
  dataset <- resolve_amc_dataset(dataset)
  state <- prepare_amc_dataset(dataset)
  if (state$status != amc_dataset_status$ready) {
    stop("Dataset ", dataset$title, " is not ready")
  }
  func <- dataset$read
  if (is.null(func)) {
    stop("dataset reader not set")
  }
  func()
}
