test_that("metuk_hadcet_monthly_dataset is valid", {
  expect_valid_amc_dataset(metuk_hadcet_monthly_dataset)
})

test_that("read_metuk_hadcet_monthly returns valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(metuk_hadcet_monthly_dataset))
})

test_that("metuk_hadcet_yearly_dataset is valid dataset", {
  expect_valid_amc_dataset(metuk_hadcet_yearly_dataset)
})

test_that("read_metuk_hadcet_yearly returns valid AMC tibble", {
  expect_valid_amc_tibble(read_amc_dataset(metuk_hadcet_yearly_dataset))
})

