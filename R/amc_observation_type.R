amc_observation_type <- function(code, name) {
  list(
    type = "observation_type",
    format = "observation_type",
    code = code,
    name = name
  )
}

#' Get observation types
#'
#' @return Vector of observation types
#' @export
#'
#' @examples
#' get_amc_observation_type()
get_amc_observation_type <- function() {
  amc_observation_type("max_temp", "Maximum temperature")
}
