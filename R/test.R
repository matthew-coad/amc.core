#' Datasource used to perform internal testing of amc
test_datasource <- new_amc_datasource(
  "test",
  "AMC testing datasource",
  "https://example.com"
)

#' Dataset that performs a very quick download
test_quick_dataset <- new_amc_dataset(
  test_datasource,
  "test_quick",
  "Quick download test"
)

test_quick_filename <- "test_quick.csv"

#' Download test dataset 1
download_test_quick_dataset <- function() {
  test_quick_filepath <- get_amc_dataset_path(test_quick_dataset, test_quick_filename)
  fileConn <- file(test_quick_filepath)
  data_lines <- c(
    "year,max_temperature",
    "2000,10.1",
    "2001,10.2"
  )
  writeLines(data_lines, fileConn)
  close(fileConn)
}

#' Read test dataset 1
read_test_quick_dataset <- function() {
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

#' Read test dataset 1
read_test_downloadfail_dataset <- function() {
  tibble::tibble(
    year = c(2000, 2001),
    max_temperature = c(10.1, 10.2)
  )
}
