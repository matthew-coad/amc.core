test_that("read tempr_nhtr returns valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(tempr_nhtr))
})
