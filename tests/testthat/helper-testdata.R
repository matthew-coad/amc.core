import_test_data <- function(dataset, filename) {
  import_amc_dataset_file(dataset, testthat::test_path("testdata", filename))
}

