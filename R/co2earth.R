
#' Earths CO2 website
#'
#' @export
#'
#' @examples
#' co2earth_datasource_code
co2earth_datasource_code <- "co2earth"

#' Earths CO2 data source
#'
#' @export
#'
#' @examples
#' co2earth_datasource
co2earth_datasource <- new_amc_datasource(
  co2earth_datasource_code,
  "Earths CO2 Website",
  "https://www.co2.earth/"
)

#' IAC Switzerland Global CO2 Yearly Dataset Code
#'
#' @export
#'
#' @examples
#' co2earth_co2_iac_annual_historical_dataset_code
co2earth_co2_iac_annual_historical_dataset_code <- "co2_iac_annual_historical"

#' IAC Switzerland Global CO2 Yearly Dataset
#'
#' @export
#'
#' @examples
#' co2earth_co2_iac_annual_historical_dataset
co2earth_co2_iac_annual_historical_dataset <- new_amc_dataset(
  co2earth_datasource_code,
  co2earth_co2_iac_annual_historical_dataset_code,
  "IAC Switzerland Global, CO2 Yearly"
)

#' List all datasets available from the Co2 Earth dataset
#'
#' @return Dataset tibble
#' @export
#'
#' @examples
#' list_co2earth_dataset()
list_co2earth_dataset <- function() {
  co2earth_co2_iac_annual_historical_dataset
}

co2earth_iac_cO2_yearly_file_name <- "mole_fraction_of_carbon_dioxide_in_air_input4MIPs_GHGConcentrations_CMIP_UoM-CMIP-1-1-0_gr3-GMNHSH_0000-2014.csv"

#' Read Total Solar Irradiance Composite dataset
#'
#' @return Tibble
#' @export
#'
#'
#' @examples
#' read_co2earth_co2_iac_annual_historical_dataset()
read_co2earth_co2_iac_annual_historical_dataset <- function() {
  dataset_path <- get_amc_dataset_path(co2earth_co2_iac_annual_historical_dataset, co2earth_iac_cO2_yearly_file_name)
  cols <- readr::cols(
    year = readr::col_double(),
    data_mean_global = readr::col_double(),
    data_mean_nh = readr::col_double(),
    data_mean_sh = readr::col_double()
  )
  co2_raw <- readr::read_csv(dataset_path, col_types = cols)
  co2_long <- tidyr::pivot_longer(co2_raw, !.data$year, names_to = "zone", values_to = "co2_fraction")
  zone_map <- tibble::tibble(
    zone = c("data_mean_global", "data_mean_nh", "data_mean_sh"),
    zone_type_code = c(global_zone_type_code, hemisphere_zone_type_code, hemisphere_zone_type_code),
    zone_code = c(global_zone_code, northern_hemisphere_zone_code, southern_hemisphere_zone_code)
  )
  co2_long |>
    dplyr::inner_join(zone_map, by = "zone") |>
    dplyr::mutate(
      datasource_code = co2earth_datasource_code,
      dataset_code = co2earth_co2_iac_annual_historical_dataset_code
    ) |>
    dplyr::select(
      .data$datasource_code,
      .data$dataset_code,
      .data$zone_type_code,
      .data$zone_code,
      .data$year,
      .data$co2_fraction
    )
}

