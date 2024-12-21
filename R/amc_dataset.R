
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

new_amc_dataset <- function(datasource_code, dataset_code, dataset_name) {
  tibble::tibble(
    datasource_code = datasource_code,
    dataset_code,
    dataset_name)
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

#' List all available amc datasets
#'
#' @return Tibble
#' @export
#'
#' @examples
#' list_amc_dataset()
list_amc_dataset <- function() {
  dplyr::bind_rows(
    list_gistemp_dataset(),
    list_issi_dataset(),
    list_temprecord_dataset(),
    list_co2earth_dataset(),
    list_ncei_dataset()
  )
}

get_amc_dataset_func <- function() {
  tryCatch(match.fun("get_amc_dataset"), error = function(cond) NULL)
}
