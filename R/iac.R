#' IAC Switzerland data source
#'
#' @export
#'
#' @examples
#' iac
iac <- new_amc_datasource(
  "iac",
  "Institute for Atmospheric and Climate Science (IAC)",
  reference_code = "IAC",
  reference_url = "https://iac.ethz.ch/"
)

#' IAC Switzerland Global CO2 Yearly Dataset
#'
#' @export
#'
#' @examples
#' iac_co2_yearly
iac_co2_yearly <- new_amc_dataset(
  iac,
  "iac_co2_yearly",
  "IAC, CO2, Global, Yearly, Years 0-2014",
  "IAC Switzerland Global CO2 Yearly",
  reference_url = "https://www.co2.earth/historical-co2-datasets"
)

iac_co2_yearly_url <- "ftp://data.iac.ethz.ch/CMIP6/input4MIPs/UoM/GHGConc/CMIP/yr/atmos/UoM-CMIP-1-1-0/GHGConc/gr3-GMNHSH/v20160701/mole_fraction_of_carbon_dioxide_in_air_input4MIPs_GHGConcentrations_CMIP_UoM-CMIP-1-1-0_gr3-GMNHSH_0000-2014.csv"
iac_co2_yearly_filename <- "mole_fraction_of_carbon_dioxide_in_air_input4MIPs_GHGConcentrations_CMIP_UoM-CMIP-1-1-0_gr3-GMNHSH_0000-2014.csv"

#' Download IAC Switzerland Global CO2 Yearly Dataset
download_iac_co2_yearly <- function() {
  iac_co2_yearly_path <- get_amc_dataset_path(iac_co2_yearly, iac_co2_yearly_filename)
  utils::download.file(url = iac_co2_yearly_url, destfile = iac_co2_yearly_path, mode = "wb")
}

iac_co2_yearly$download <- download_iac_co2_yearly

#' Read IAC Switzerland Global CO2 Yearly tibble
read_iac_co2_yearly <- function() {
  dataset_path <- get_amc_dataset_path(iac_co2_yearly, iac_co2_yearly_filename)
  cols <- readr::cols(
    year = readr::col_double(),
    data_mean_global = readr::col_double(),
    data_mean_nh = readr::col_double(),
    data_mean_sh = readr::col_double()
  )
  co2_raw <- suppressMessages(readr::read_csv(dataset_path, col_types = cols))
  co2_long <- co2_raw |> tidyr::pivot_longer(!"year", names_to = "zone", values_to = "co2")
  zone_map <- tibble::tibble(
    zone = c("data_mean_global", "data_mean_nh", "data_mean_sh"),
    zone_code = c(global_zone_code, northern_hemisphere_zone_code, southern_hemisphere_zone_code)
  )
  co2_long |>
    dplyr::inner_join(zone_map, by = "zone") |>
    dplyr::select(
      "zone_code",
      "year",
      "co2"
    )
}

iac_co2_yearly$read <- read_iac_co2_yearly
