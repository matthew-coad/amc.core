test_that("new_amc_dataset returns initialized dataset", {
  test_dataset <- new_amc_dataset("test_datasource", "test_dataset", "New dataset test")
  expect_equal(test_dataset$datasource_code, "test_datasource")
  expect_equal(test_dataset$dataset_code, "test_dataset")
  expect_equal(test_dataset$dataset_name, "New dataset test")
})

test_that("New dataset class includes amc_dataset_class", {
  test_dataset <- new_amc_dataset("test_datasource", "test_dataset", "New dataset test")
  expect_contains(class(test_dataset),  amc_dataset_class)
})

test_that("list_amc_datasource includes test datasource", {
  datasource <- list_amc_datasource()
  test_datasource <- datasource |> dplyr::filter(datasource_code == test_datasource_code)
  expect_equal(test_datasource$datasource_code,  test_datasource_code)
})

test_that("prepare_amc_dataset_repository creates repository folder", {
  prepare_local_test_repository("prepare_amc_dataset_repository_creates_repository_folder")
  test_dataset_path <- get_amc_dataset_repository_path(test_dataset1)
  prepare_amc_dataset_repository(test_dataset1)
  expect_true(file.exists(test_dataset_path))
})

test_that("import_amc_dataset_file copies file to dataset", {
  prepare_local_test_repository("import_amc_dataset_file_copies_file")
  test_filename <- "Test_DataSet1.csv"
  test_filepath <- testthat::test_path("testdata", test_filename)
  expected_test_filepath <- get_amc_dataset_path(test_dataset1, test_filename)
  import_amc_dataset_file(test_dataset1, test_filepath)
  expect_true(file.exists(expected_test_filepath))
})

test_that("import_test_data helper copies file to dataset", {
  prepare_local_test_repository("import_test_data_helper_copies_file")
  test_filename <- "Test_DataSet1.csv"
  expected_test_filepath <- get_amc_dataset_path(test_dataset1, test_filename)
  import_test_data(test_dataset1, test_filename)
  expect_true(file.exists(expected_test_filepath))
})

