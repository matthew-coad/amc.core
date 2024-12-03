test_that("multiplication works", {
  testthat::expect_contains(get_amc_observation_type()$code, "max_temp")
})
