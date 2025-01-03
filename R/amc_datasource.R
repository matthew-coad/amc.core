
#' Class that indicates an object is an amc datasource
#'
#' @export
#'
#' @examples
#' amc_dataset_class %in% class(amctest)
amc_datasource_class <- "amc_datasource"

#' Create new AMC data source
#'
#' @param name Name of the data source. Must be a unique short alpha+numeric lower case string
#' @param title Data source title Usually the name of the organization and reference code.
#' @param reference_code Optional short code used to refer to the data source. Often associated publically with the owing organization.
#' @param reference_url URL to a reference guide to the data source. Usually the owning organizations website.
#'
#' @return AMC data source list
#' @export
#'
#' @examples
#'new_amc_datasource("test_datasource", "TEST - AMC test resources")
new_amc_datasource <- function(name, title, reference_code = "", reference_url = "") {
  datasource <- list(
    name = name,
    title = title,
    reference_code = reference_code,
    reference_url = reference_url)
  class(datasource) <- c(amc_datasource_class, class(datasource))
  datasource
}

#' List all available amc datasources
#'
#' @return List of all visible amc data sources
#' @export
#'
#' @examples
#' \donttest{
#'   list_amc_datasource()
#' }
list_amc_datasource <- function() {
  packages <- base::search() |> purrr::discard(\(x) x == ".GlobalEnv" | x == "Autoloads")
  package_object_names <- packages |>
    purrr::map(\(x) ls(x)) |>
    unlist(recursive = TRUE)
  object_names <- package_object_names |> unique()
  datasources <- object_names |>
    purrr::map(\(x) get(x)) |>
    purrr::keep(\(x) amc_datasource_class %in% class(x))
  datasource_names <- datasources |> purrr::map(\(x) x$name ) |> unlist()
  datasources <- datasources |> purrr::set_names(nm = datasource_names)
  datasources
}

#' View available AMC data sources as a dataframe
#'
#' @return Data frame containing data sources name, synopsis and reference title + url
#' @export
#'
#' @examples
#' \donttest{
#'   view_amc_datasource()
#' }
view_amc_datasource <- function() {
  datasources <- list_amc_datasource()
  tibble::tibble(
    name = unlist(purrr::map(datasources, "name")),
    title = unlist(purrr::map(datasources, "title")),
    reference_code = unlist(purrr::map(datasources, "reference_code")),
    reference_url = unlist(purrr::map(datasources, "reference_url")),
  )
}
