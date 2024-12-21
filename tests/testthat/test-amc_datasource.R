test_that("New datasource returns initialized datasource", {
  test_datasource <- new_amc_datasource("new_datasource_test", "New datasource test", "http://newdatasource.com")
  expect_equal(test_datasource$datasource_code, "new_datasource_test")
  expect_equal(test_datasource$datasource_name, "New datasource test")
  expect_equal(test_datasource$datasource_reference_url, "http://newdatasource.com")
})

test_that("New datasource class includes amc_dataset_class", {
  test_datasource <- new_amc_datasource("new_datasource_test", "New datasource test", "http://newdatasource.com")
  expect_contains(class(test_datasource),  amc_datasource_class)
})

test_that("list_amc_datasource includes test datasource", {
  datasource <- list_amc_datasource()
  test_datasource <- datasource |> dplyr::filter(datasource_code == test_datasource_code)
  expect_equal(test_datasource$datasource_code,  test_datasource_code)
})
