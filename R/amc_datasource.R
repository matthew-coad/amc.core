
#' Class that indicates an object is an amc datasource
#'
#' @export
#'
#' @examples
#' amc_datasource_class %in% class(test_dataset1)
amc_datasource_class <- "amc_datasource"

#' Create new amc datasource
#'
#' @param datasource_code Code that uniquely identifies the datasource
#' @param datasource_name Name of the datasource
#' @param datasource_reference_url Url to a reference or guide to the datasource
#'
#' @return Tibble
#' @export
#'
#' @examples
#'new_amc_datasource("test_datasource", "Test datasource", "https://example.com/")
new_amc_datasource <- function(datasource_code, datasource_name, datasource_reference_url) {
  datasource <- tibble::tibble(datasource_code, datasource_name, datasource_reference_url)
  class(datasource) <- c(amc_datasource_class, class(datasource))
  datasource
}

#' List all available amc datasources
#'
#' @return Tibble
#' @export
#'
#' @examples
#' list_amc_datasource()
list_amc_datasource <- function() {
  packages <- base::search() |> purrr::discard(\(x) x == ".GlobalEnv" | x == "Autoloads")
  amc_datasource_names <- packages |>
    purrr::map(ls) |>
    unlist(recursive = TRUE) |>
    unique()
  amc_datasource <- amc_datasource_names |>
    purrr::map(get) |>
    purrr::keep(\(x)amc_datasource_class %in% class(x))
  dplyr::bind_rows(amc_datasource)
}
