
#' IAC datasource code
#'
#' @export
#'
#' @examples
#' iac_datasource_code
iac_datasource_code <- "iac"

#' IAC data source
#'
#' @export
#'
#' @examples
#' iac_datasource
iac_datasource <- new_amc_datasource(
  iac_datasource_code,
  "IAC Switzerland",
  "https://www.co2.earth/"
)

#' IAC Switzerland Annual historical CO2 Dataset Code
#'
#' @export
#'
#' @examples
#' iac_annual_historical_co2_code
iac_annual_historical_co2_code <- "iac_annual_historical_co2"

#' IAC Switzerland Annual Historical Dataset
#'
#' @export
#'
#' @examples
#' iac_annual_historical_co2_dataset
iac_annual_historical_co2_dataset <- new_amc_dataset(
  iac_datasource_code,
  iac_annual_historical_co2_code,
  "IAC Switzerland Annual, Historical CO2"
)

iac_annual_historical_co2_filename <- "mole_fraction_of_carbon_dioxide_in_air_input4MIPs_GHGConcentrations_CMIP_UoM-CMIP-1-1-0_gr3-GMNHSH_0000-2014.csv"

#' Read IAC Switzerland Annual Historical Dataset
#'
#' @return Tibble
#' @export
#'
#' @examples
#' read_iac_annual_historical_co2()
read_iac_annual_historical_co2 <- function() {
  dataset_path <- get_amc_dataset_path(iac_annual_historical_co2_dataset, iac_annual_historical_co2_filename)
  cols <- readr::cols(
    year = readr::col_double(),
    data_mean_global = readr::col_double(),
    data_mean_nh = readr::col_double(),
    data_mean_sh = readr::col_double()
  )
  co2_raw <- readr::read_csv(dataset_path, col_types = cols)
  co2_long <- tidyr::pivot_longer(co2_raw, !.data$year, names_to = "zone", values_to = "co2")
  zone_map <- tibble::tibble(
    zone = c("data_mean_global", "data_mean_nh", "data_mean_sh"),
    zone_code = c(global_zone_code, northern_hemisphere_zone_code, southern_hemisphere_zone_code)
  )
  co2_long |>
    dplyr::inner_join(zone_map, by = "zone") |>
    dplyr::select(
      .data$zone_code,
      .data$year,
      .data$co2
    )
}
