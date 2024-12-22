#' Class that indicates an object is an amc dataset
#'
#' @export
#'
#' @examples
#' amc_dataset_class %in% class(test_dataset1)
amc_dataset_class <- "amc_dataset"

#' Create new amc dataset
#'
#' @param datasource_code Code of owning data source
#' @param dataset_code Unique dataset code
#' @param dataset_name Data set name
#'
#' @returns Dataset tibble
#' @export
#'
#' @examples
#' new_amc_dataset(test_datasource_code, test_dataset1_code, "Test dataset1")
new_amc_dataset <- function(datasource_code, dataset_code, dataset_name) {
  dataset <- tibble::tibble(
    datasource_code = datasource_code,
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
#' prepare_amc_dataset_repository(gistemp_zonal_annual_means_dataset)
prepare_amc_dataset_repository <- function(dataset) {
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
#' import_amc_dataset_file(gistemp_zonal_annual_means_dataset, "ZonAnn.Ts+dSST.csv")
import_amc_dataset_file <- function(dataset, filepath) {
  repository_path <- prepare_amc_dataset_repository(dataset)
  file.copy(filepath, repository_path, overwrite=TRUE)
}

get_amc_dataset_func <- function() {
  tryCatch(match.fun("get_amc_dataset"), error = function(cond) NULL)
}

get_amc_dataset <- function(datasource) {
  func_name <- paste0("get_", datasource$datasource_code, "_dataset")
  dataset_func <- tryCatch(match.fun(func_name), error = function(cond) NULL)
  if (!is.null(dataset_func)) {
    dataset_func(datasource)
  } else {
    tibble::tibble(
      datasource_code = character(),
      dataset_code = character()
    )
  }
}

