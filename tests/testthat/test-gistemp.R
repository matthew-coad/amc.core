test_that("gistemp_dataset_code_set", {
  expect_equal(gistemp_datasource_code, "gistemp")
})

test_that("amc_gistemp_datasource available", {
  expect_equal(gistemp_datasource$datasource_code, gistemp_datasource_code)
})
