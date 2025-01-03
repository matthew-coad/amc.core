test_that("New datasource returns initialized datasource", {
  test_datasource <- new_amc_datasource("new_datasource_test", "New datasource test", reference_code = "TEST", reference_url = "http://newdatasource.com")
  expect_equal(test_datasource$name, "new_datasource_test")
  expect_equal(test_datasource$title, "New datasource test")
  expect_equal(test_datasource$reference_code, "TEST")
  expect_equal(test_datasource$reference_url, "http://newdatasource.com")
})

test_that("New datasource class includes amc_datasource_class", {
  test_datasource <- new_amc_datasource("new_datasource_test", "New datasource test", "http://newdatasource.com")
  expect_contains(class(test_datasource),  amc_datasource_class)
})

test_that("list_amc_datasource includes test dataset", {
  datasources <- list_amc_datasource()
  actual_datasource <- datasources |> purrr::keep(\(x) x$name == amctest$name) |> purrr::pluck(1)
  expect_equal(actual_datasource$name,  amctest$name)
})
