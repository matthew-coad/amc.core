test_that("read issi_tsicomp returns valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(issi_tsicomp))
})
