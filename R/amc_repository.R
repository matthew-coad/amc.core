
#' Get the default AMC Repository path.
#'
#' This is the path used to store locally cached amc data if not specifically overridden by the
#' "amc.repository.path" option.
#'
#' @returns File path as a character value
#' @export
#'
#' @examples
#' get_amc_default_repository_path()
get_amc_default_repository_path <- function() {
  file.path(tools::R_user_dir("amc", which = "data"))
}

#' Get AMC Repository path.
#'
#' AMC data will downloaded/read from within this data repository.
#'
#' @returns File path as a character value
#' @export
#'
#' @examples
#' get_amc_repository_path()
get_amc_repository_path <- function() {
  getOption("amc.repository.path", get_amc_default_repository_path())
}

#' Get Path used to store data related to a specific datasource
#'
#' @param datasource Target datasource
#'
#' @return File path
#' @export
#'
#' @examples
#' get_amc_datasource_repository_path(test_dataset1)
get_amc_datasource_repository_path <- function(datasource) {
  file.path(get_amc_repository_path(), datasource$datasource_code)
}

#' Get Path used to store data related to a specific dataset
#'
#' @param dataset Target dataset
#'
#' @return File path as a character value
#' @export
#'
#' @examples
#' get_amc_dataset_repository_path(test_dataset1)
get_amc_dataset_repository_path <- function(dataset) {
  file.path(get_amc_repository_path(), dataset$datasource_code, dataset$dataset_code)
}

#' Get path to a file within an amc dataset
#'
#' @param dataset Target dataset
#' @param filename Files filename
#'
#' @return Character vector
#' @export
#'
#' @examples
#' get_amc_dataset_path(test_dataset1, "Test1.csv")
get_amc_dataset_path <- function(dataset, filename) {
  file.path(get_amc_dataset_repository_path(dataset), filename)
}
