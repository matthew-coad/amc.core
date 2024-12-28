test_that("ncei_tsi_dataset is valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(ncei_tsi_dataset))
})
