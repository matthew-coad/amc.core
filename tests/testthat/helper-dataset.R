#' Datasource used to perform internal testing of amc
test_datasource <- new_amc_datasource(
  "test",
  "AMC testing datasource",
  "https://example.com"
)

#' Dataset that performs a very quick download
test_dataset1 <- new_amc_dataset(
  test_datasource,
  "test_test1",
  "Test dataset1"
)

test_datafile1_filename <- "Test1.csv"

#' Download test dataset 1
download_test_test1_dataset <- function() {
  test_datafile1_filepath <- get_amc_dataset_path(test_dataset1, test_datafile1_filename)
  fileConn<-file(test_datafile1_filepath)
  data_lines <- c(
    "year,max_temperature",
    "2000,10.1",
    "2001,10.2"
  )
  writeLines(data_lines, fileConn)
  close(fileConn)
}

#' Read test dataset 1
read_test_test1_dataset <- function() {
  tibble::tibble(
    year = c(2000, 2001),
    max_temperature = c(10.1, 10.2)
  )
}

#' Dataset that will always fail to download
test_downloadfail_dataset <- new_amc_dataset(
  test_datasource,
  "test_downloadfail",
  "Test download fail"
)

#' Download test dataset 1
download_test_downloadfail_dataset <- function() {
  stop("Failed to download on purpose")
}

#' Test environment that includes test datasets
test_envir <- environment()

#' Read test dataset 1
read_test_downloadfail_dataset <- function() {
  tibble::tibble(
    year = c(2000, 2001),
    max_temperature = c(10.1, 10.2)
  )
}

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
