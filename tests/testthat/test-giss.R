test_that("read_giss_ta_zonal_yearly returns valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(giss_ta_zonal_yearly_dataset))
})
