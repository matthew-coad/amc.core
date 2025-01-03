test_that("read ncei_tsi returns valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(ncei_tsi))
})
