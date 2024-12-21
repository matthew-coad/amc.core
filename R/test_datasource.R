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
test_dataset1_code <- "test1"

#' Dataset used to perform internal testing of AMC
#'
#' @export
#'
#' @examples
#' test_datasource
test_dataset1 <- new_amc_dataset(
  test_datasource_code,
  test_dataset1_code,
  "Test dataset1"
)
