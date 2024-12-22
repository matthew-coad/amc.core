test_that("New datasource returns initialized datasource", {
  test_datasource <- new_amc_datasource("new_datasource_test", "New datasource test", "http://newdatasource.com")
  expect_equal(test_datasource$datasource_code, "new_datasource_test")
  expect_equal(test_datasource$datasource_name, "New datasource test")
  expect_equal(test_datasource$datasource_reference_url, "http://newdatasource.com")
})

test_that("New datasource class includes amc_datasource_class", {
  test_datasource <- new_amc_datasource("new_datasource_test", "New datasource test", "http://newdatasource.com")
  expect_contains(class(test_datasource),  amc_datasource_class)
})

test_that("list_amc_datasource includes test dataset", {
  datasets <- list_amc_dataset()
  test_dataset <- datasets |> dplyr::filter(datasource_code == test_datasource_code & dataset_code == test_dataset1_code)
  expect_equal(test_dataset$datasource_code,  test_datasource_code)
  expect_equal(test_dataset$dataset_code,  test_dataset1_code)
})
