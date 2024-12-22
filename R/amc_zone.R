#' Global zone type code
#'
#' @export
#'
#' @examples
#' global_zone_type_code
global_zone_type_code <- "global"

#' Global zone code
#'
#' @export
#'
#' @examples
#' global_zone_code
global_zone_code <- "global"

#' Hemisphere zone type code
#'
#' @export
#'
#' @examples
#' hemisphere_zone_type_code
hemisphere_zone_type_code <- "hemisphere"

#' Northern Hemisphere zone code
#'
#' @export
#'
#' @examples
#' northern_hemisphere_zone_code
northern_hemisphere_zone_code <- "northern_hemisphere"

#' Southern Hemisphere zone code
#'
#' @export
#'
#' @examples
#' southern_hemisphere_zone_code
southern_hemisphere_zone_code <- "southern_hemisphere"

#' list zone type codes
#'
#' @returns Tibble containing zone_type_code and zone_type_name columns
#' @export
#'
#' @examples
#' list_zone_type()
list_zone_type <- function() {
  tibble::tibble(
    zone_type_code = c(global_zone_type_code, hemisphere_zone_type_code),
    zone_type_name = c("Global", "Hemisphere")
  )
}

#' list zone codes
#'
#' @returns Tibbler containing zone_type_code, zone_code and zone_name columns
#' @export
#'
#' @examples
#' list_zone()
list_zone <- function() {
  tibble::tibble(
    zone_type_code = c(global_zone_type_code, hemisphere_zone_type_code, hemisphere_zone_type_code),
    zone_type = c(global_zone_code, northern_hemisphere_zone_code, southern_hemisphere_zone_code),
    zone_type_name = c("Global", "Northern Hemisphere", "Southern Hemisphere")
  )
}

