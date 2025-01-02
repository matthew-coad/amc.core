

valid_tibble_colnames <-
  c(
    "year",
    "month",
    "date",

    "zone_type_code",
    "zone_code",

    "temperature_anomaly",
    "mean_temperature",
    "co2",
    "solar_irradiance",
    "solar_irradiance_uncertainty"
  )

expect_valid_amc_tibble <- function(object) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect()
  act$invalid_colnames <- base::setdiff(colnames(act$val), base::intersect(valid_tibble_colnames, colnames(act$val)))
  expect (
    length(act$invalid_colnames) == 0,
    sprintf("Invalid column(s) %s in amc tibble", paste(act$invalid_colnames, collapse = ", "))
  )

  # 3. Invisibly return the value
  invisible(act$val)
}
