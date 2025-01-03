test_that("read iac_co2_yearly return valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(iac_co2_yearly))
})

