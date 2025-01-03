test_that("read metuk_hadcet_monthly returns valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(metuk_hadcet_monthly))
})

test_that("read metuk_hadcet_yearly returns valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(metuk_hadcet_yearly))
})
