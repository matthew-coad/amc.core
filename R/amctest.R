#' Datasource used to perform internal testing of amc
amctest <- new_amc_datasource(
  "amctest",
  "AMC Testing resources",
  reference_code = "AMCTEST",
  reference_url = "https://example.com"
)

#' Dataset that performs a very quick download
amctest_quick <- new_amc_dataset(
  amctest,
  "amctest_quick",
  "AMCTEST, General Test",
  "Quick download test"
)

amctest_quick_filename <- "test_quick.csv"

#' Download test dataset 1
download_amctest_quick <- function() {
  filepath <- get_amc_dataset_path(amctest_quick, amctest_quick_filename)
  fileConn <- file(filepath)
  data_lines <- c(
    "year,max_temperature",
    "2000,10.1",
    "2001,10.2"
  )
  writeLines(data_lines, fileConn)
  close(fileConn)
}

amctest_quick$download <- download_amctest_quick

read_amctest_quick <- function() {
  tibble::tibble(
    year = c(2000, 2001),
    max_temperature = c(10.1, 10.2)
  )
}

amctest_quick$read <- read_amctest_quick

#' Dataset that will always fail to download
amctest_downloadfail <- new_amc_dataset(
  amctest,
  "amctest_downloadfail",
  "AMCTEST, Download fail Test",
  "Download fail test"
)

download_amctest_downloadfail <- function() {
  stop("Failed to download on purpose")
}

amctest_downloadfail$download <- download_amctest_downloadfail
