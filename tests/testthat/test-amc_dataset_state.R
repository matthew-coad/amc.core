test_that("read_amc_dataset_state returns missing if state not saved", {
  prepare_local_test_repository("read_amc_dataset_state_missing")
  state <- read_amc_dataset_state(amctest_quick)
  expect_equal(state$status, amc_dataset_status$missing)
})

test_that("save_amc_dataset_state saves state", {
  prepare_local_test_repository("save_amc_dataset_state_updates_state")
  saved_state <- save_amc_dataset_state(amctest_quick, amc_dataset_status$ready)
  state <- read_amc_dataset_state(amctest_quick)
  expect_equal(saved_state$status, amc_dataset_status$ready)

  expect_equal(state$datasource$name, amctest_quick$datasource$name)
  expect_equal(state$name, amctest_quick$name)
  expect_equal(state$status, amc_dataset_status$ready)
})


