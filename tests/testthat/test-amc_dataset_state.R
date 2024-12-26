test_that("read_amc_dataset_state returns missing if state not saved", {
  prepare_local_test_repository("read_amc_dataset_state_missing")
  state <- read_amc_dataset_state(test_dataset1)
  expect_equal(state$status, amc_dataset_status$missing)
})

test_that("save_amc_dataset_state saves state", {
  prepare_local_test_repository("save_amc_dataset_state_updates_state")
  saved_state <- save_amc_dataset_state(test_dataset1, amc_dataset_status$ready)
  state <- read_amc_dataset_state(test_dataset1)
  expect_equal(saved_state$status, amc_dataset_status$ready)

  expect_equal(state$datasource_code, test_dataset1$datasource_code)
  expect_equal(state$dataset_code, test_dataset1$dataset_code)
  expect_equal(state$status, amc_dataset_status$ready)
})


