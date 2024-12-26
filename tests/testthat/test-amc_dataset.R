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

test_that("get_download_amc_dataset gets func if it exists", {
  func <- get_download_amc_dataset_func(test_dataset1)
  expect_false(is.null(func))
})

test_that("get_download_amc_dataset returns null if it doesnt exist", {
  test_dataset <- new_amc_dataset("test_datasource", "test_dataset", "New dataset test")
  func <- get_download_amc_dataset_func(test_dataset)
  expect_null(func)
})

test_that("get_download_amc_dataset fails if its required and doesnt exist", {
  test_dataset <- new_amc_dataset("test_datasource", "test_dataset", "New dataset test")
  expect_error(get_download_amc_dataset_func(test_dataset, required = TRUE), regexp = "dataset downloader download_test_dataset_dataset not found")
})

test_that("download_amc_dataset invokes dataset download func", {
  prepare_local_test_repository("download_amc_dataset_invokes_func")
  test_datafile_path <- get_amc_dataset_path(test_dataset1, test_datafile1_filename)

  download_amc_dataset(test_dataset1)
  expect_true(file.exists(test_datafile_path))
})

test_that("run_download_amc_dataset updates prepare_date", {
  prepare_local_test_repository("download_amc_dataset_updates_prepare_data")
  run_download_amc_dataset(test_dataset1, {})
  run1 <- read_amc_dataset_state(test_dataset1)
  # Wait
  Sys.sleep(0.1)
  run_download_amc_dataset(test_dataset1, {})
  run2 <- read_amc_dataset_state(test_dataset1)

  prepare_interval <- abs(lubridate::interval(run1$prepare_date, run2$prepare_date) / lubridate::seconds(1))
  expect_gt(prepare_interval, 0.01)
})

test_that("run_download_amc_dataset exception sets download failed status", {
  prepare_local_test_repository("download_amc_dataset_exception_set_failed_status")
  suppressMessages(run_download_amc_dataset(test_dataset1, { stop("expected download failure") }))
  run1 <- read_amc_dataset_state(test_dataset1)

  expect_equal(run1$status, amc_dataset_status$download_failed)
  expect_null(run1$prepare_date)
  expect_equal(run1$fail_condition$message, "expected download failure")
})

test_that("prepare_amc_dataset runs successfully", {
  prepare_local_test_repository("prepare_amc_dataset_successful")
  test_dataset_path <- get_amc_dataset_repository_path(test_dataset1)
  test_datafile_path <- get_amc_dataset_path(test_dataset1, test_datafile1_filename)

  state <- prepare_amc_dataset(test_dataset1)
  prepare_hours_ago <- abs(lubridate::interval(state$prepare_date, lubridate::now()) / lubridate::hours(1))

  expect_true(file.exists(test_dataset_path))
  expect_true(file.exists(test_datafile_path))
  expect_equal(state$status, amc_dataset_status$ready)
  expect_null(state$fail_condition)
  expect_lt(prepare_hours_ago, 1)

})

test_that("Multiple prepare_amc_dataset downloads data once", {
  prepare_local_test_repository("multiple_prepare_amc_dataset_downloads_once")

  state1 <- prepare_amc_dataset(test_dataset1)
  # Wait to ensure time change
  Sys.sleep(0.1)
  state2 <- prepare_amc_dataset(test_dataset1)

  expect_equal(state1$status, amc_dataset_status$ready)
  expect_equal(state2$status, amc_dataset_status$ready)
  expect_null(state1$fail_condition)
  expect_null(state2$fail_condition)
  expect_equal(state1$prepare_date, state2$prepare_date)

})
