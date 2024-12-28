test_that("issi_tsicomp_dataset is valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(issi_tsicomp_dataset))
})
