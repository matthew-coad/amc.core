expect_download_amc_dataset_func <- function(object) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect()
  act$func <- get_download_amc_dataset_func(act$val, required = FALSE)
  expect (
    !is.null(act$func),
    sprintf("No download func found for dataset %s", act$val$dataset_name)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}

expect_read_amc_dataset_func <- function(object) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect()
  act$func <- get_read_amc_dataset_func(act$val, required = FALSE)
  expect (
    !is.null(act$func),
    sprintf("No read func found for dataset %s", act$val$dataset_name)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}

expect_valid_amc_dataset <- function(object) {
  expect_download_amc_dataset_func(object)
  expect_read_amc_dataset_func(object)
}

valid_tibble_colnames <-
  c(
    "year",
    "month",
    "date",

    "zone_code",

    "mean_temperature",
    "co2"
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
