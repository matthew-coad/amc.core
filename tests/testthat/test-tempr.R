test_that("tempr_nhtr_dataset is valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(tempr_nhtr_dataset))
})
