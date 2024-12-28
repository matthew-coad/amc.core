test_that("iac_co2_yearly_dataset return valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(iac_co2_yearly_dataset))
})

