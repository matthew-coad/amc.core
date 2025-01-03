test_that("Returns a default repository path", {
  expect_equal(is.null(get_amc_default_repository_path()), FALSE)
})

test_that("Repository path defaults to the default repository path", {
  expect_equal(get_amc_repository_path(), get_amc_default_repository_path())
})

test_that("Setting repository path option changes the repository path", {
  test_repository_path <- "test_repository_path"
  options <- options(amc.repository.path = test_repository_path)
  withr::defer(options(options))
  expect_equal(get_amc_repository_path(), test_repository_path)
})

test_that("prepare_local_test_repository correctly changes the repository path", {
  test_name <- "prepare_local_test_repository_changes_path"
  test_repository_path <- prepare_local_test_repository(test_name)
  expect_equal(get_amc_repository_path(), test_repository_path)
})

test_that("Datasource repository path is child path of amc repository", {
  test_name <- "datasource_path_is_repository_child"
  test_repository_path <- prepare_local_test_repository(test_name)
  expected_datasource_path <- file.path(test_repository_path, amctest$name)
  expect_equal(get_amc_datasource_repository_path(amctest), expected_datasource_path)
})

test_that("Dataset repository path is child path of datasource repository", {
  test_name <- "dataset_repository_path_is_datasource_child"
  test_repository_path <- prepare_local_test_repository(test_name)
  expected_dataset_path <- file.path(test_repository_path, amctest$name, amctest_quick$name)
  expect_equal(get_amc_dataset_repository_path(amctest_quick), expected_dataset_path)
})

test_that("Dataset path is child path of dataset repository", {
  test_name <- "dataset_path_is_datasource_child"
  test_filename <- "test_filename"
  test_repository_path <- prepare_local_test_repository(test_name)
  expected_dataset_path <- file.path(test_repository_path, amctest$name, amctest_quick$name, test_filename)
  expect_equal(get_amc_dataset_path(amctest_quick, test_filename), expected_dataset_path)
})

