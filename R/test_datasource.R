#' Test datasource code
#'
#' @export
#'
#' @examples
#' test_datasource_code
test_datasource_code <- "test"

#' Datasource used to perform internal testing of amc
#'
#' @export
#'
#' @examples
#' test_datasource
test_datasource <- new_amc_datasource(
  test_datasource_code,
  "AMC Test Datasource",
  "https://example.com"
)

#' Test dataset1 code
#'
#' @export
#'
#' @examples
#' test_dataset1_code
test_dataset1_code <- "test_test1"

#' Dataset used to perform internal testing of AMC
#'
#' @export
#'
#' @examples
#' test_dataset1
test_dataset1 <- new_amc_dataset(
  test_datasource_code,
  test_dataset1_code,
  "Test dataset1"
)

test_datafile1_filename <- "Test1.csv"

#' Download test dataset 1
#'
#' @returns Final dataset state
#' @export
#'
#' @examples
#' download_test_test1_dataset()
download_test_test1_dataset <- function() {
  run_download_amc_dataset(test_dataset1, {
    test_datafile1_filepath <- get_amc_dataset_path(test_dataset1, test_datafile1_filename)
    fileConn<-file(test_datafile1_filepath)
    data_lines <- c(
      "year,max_temperature",
      "2000,10.1",
      "2001,10.2"
    )
    writeLines(data_lines, fileConn)
    close(fileConn)
  })
}

#' Read test dataset 1
#'
#' @returns Test dataset 1 AMC tibble
#' @export
#'
#' @examples
#' read_test_test1()
read_test_test1 <- function() {
  tibble::tibble(
    year = c(2000, 2001),
    max_temperature = c(10.1, 10.2)
  )
}
