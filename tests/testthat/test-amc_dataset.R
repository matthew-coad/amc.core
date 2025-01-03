test_that("new_amc_dataset returns initialized dataset", {
  test_datasource <- new_amc_datasource("test_datasource", "Test datasource Synopsis")
  test_dataset <- new_amc_dataset(test_datasource, "test_dataset", "Test dataset synopsis", "Test dataset title", reference_code = "AMCTEST_DATASET", reference_url = "http:\\example.com")
  expect_equal(test_dataset$datasource$name, "test_datasource")
  expect_equal(test_dataset$name, "test_dataset")
  expect_equal(test_dataset$synopsis, "Test dataset synopsis")
  expect_equal(test_dataset$reference_code, "AMCTEST_DATASET")
  expect_equal(test_dataset$reference_url, "http:\\example.com")
})

test_that("New dataset class includes amc_dataset_class", {
  test_datasource <- new_amc_datasource("test_datasource", "Test datasource Synopsis")
  test_dataset <- new_amc_dataset(test_datasource, "test_dataset", "Test dataset synopsis", "Test dataset title", reference_code = "AMCTEST_DATASET", reference_url = "http:\\example.com")
  expect_contains(class(test_dataset),  amc_dataset_class)
})

test_that("list_amc_dataset includes test dataset", {
  datasets <- list_amc_dataset()
  dataset <- datasets |> purrr::keep(\(x) x$name == amctest_quick$name) |> purrr::pluck(1)
  expect_equal(dataset$name,  amctest_quick$name)
})

test_that("resolve_amc_dataset via character returns expected dataset", {
  dataset <- resolve_amc_dataset("amctest_quick")
  expect_equal(dataset$name, "amctest_quick")
})

test_that("resolve_amc_dataset using dataset just returns dataset", {
  dataset <- resolve_amc_dataset(amctest_quick)
  expect_equal(dataset$name, "amctest_quick")
})

test_that("prepare_amc_dataset_repository creates repository folder", {
  prepare_local_test_repository("prepare_amc_dataset_repository_creates_repository_folder")
  test_dataset_path <- get_amc_dataset_repository_path(amctest_quick)
  prepare_amc_dataset_repository(amctest_quick)
  expect_true(file.exists(test_dataset_path))
})

test_that("import_amc_dataset_file copies file to dataset", {
  prepare_local_test_repository("import_amc_dataset_file_copies_file")
  test_filename <- "test_quick.csv"
  test_filepath <- testthat::test_path("testdata", test_filename)
  expected_test_filepath <- get_amc_dataset_path(amctest_quick, test_filename)
  import_amc_dataset_file(amctest_quick, test_filepath)
  expect_true(file.exists(expected_test_filepath))
})

test_that("import_test_data helper copies file to dataset", {
  prepare_local_test_repository("import_test_data_helper_copies_file")
  test_filename <- "test_quick.csv"
  expected_test_filepath <- get_amc_dataset_path(amctest_quick, test_filename)
  import_test_data(amctest_quick, test_filename)
  expect_true(file.exists(expected_test_filepath))
})

test_that("download_amc_dataset invokes dataset download func", {
  prepare_local_test_repository("download_amc_dataset_invokes_func")
  test_datafile_path <- get_amc_dataset_path(amctest_quick, amctest_quick_filename)
  download_amc_dataset(amctest_quick)
  expect_true(file.exists(test_datafile_path))
})

test_that("download_amc_dataset updates prepare_date", {
  prepare_local_test_repository("download_amc_dataset_updates_prepare_date")
  download_amc_dataset(amctest_quick)
  run1 <- read_amc_dataset_state(amctest_quick)
  # Wait
  Sys.sleep(0.1)
  download_amc_dataset(amctest_quick)
  run2 <- read_amc_dataset_state(amctest_quick)

  prepare_interval <- abs(lubridate::interval(run1$prepare_date, run2$prepare_date) / lubridate::seconds(1))
  expect_gt(prepare_interval, 0.01)
})

test_that("run_download_amc_dataset exception sets download failed status", {
  prepare_local_test_repository("download_amc_dataset_exception_set_failed_status")
  suppressMessages(download_amc_dataset(amctest_downloadfail))
  run1 <- read_amc_dataset_state(amctest_downloadfail)

  expect_equal(run1$status, amc_dataset_status$download_failed)
  expect_null(run1$prepare_date)
  expect_equal(run1$fail_condition$message, "Failed to download on purpose")
})

test_that("prepare_amc_dataset runs successfully", {
  prepare_local_test_repository("prepare_amc_dataset_successful")
  test_dataset_path <- get_amc_dataset_repository_path(amctest_quick)
  test_datafile_path <- get_amc_dataset_path(amctest_quick, amctest_quick_filename)

  state <- prepare_amc_dataset(amctest_quick)
  prepare_hours_ago <- abs(lubridate::interval(state$prepare_date, lubridate::now()) / lubridate::hours(1))

  expect_true(file.exists(test_dataset_path))
  expect_true(file.exists(test_datafile_path))
  expect_equal(state$status, amc_dataset_status$ready)
  expect_null(state$fail_condition)
  expect_lt(prepare_hours_ago, 1)

})

test_that("Multiple prepare_amc_dataset downloads data once", {
  prepare_local_test_repository("multiple_prepare_amc_dataset_downloads_once")

  state1 <- prepare_amc_dataset(amctest_quick)
  # Wait to ensure time change
  Sys.sleep(0.1)
  state2 <- prepare_amc_dataset(amctest_quick)

  expect_equal(state1$status, amc_dataset_status$ready)
  expect_equal(state2$status, amc_dataset_status$ready)
  expect_null(state1$fail_condition)
  expect_null(state2$fail_condition)
  expect_equal(state1$prepare_date, state2$prepare_date)

})
