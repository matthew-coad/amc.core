#' Get repository path that is local to an individual test run
#'
#' @param test_name Name of the test
#'
#' @returns File path as a character vector
#'
#' @examples
#' local_test_repository_path("demo_test")
get_local_test_repository_path <- function(test_name) {
  file.path(tools::R_user_dir("amc_test", which = "data"), "local", test_name)
}

#' Prepare a local test repository to use with a test.
#'
#' This function prepares a new repository specific to this test and sets it
#' as the repository for the scope of the test.
#'
#' @param test_name Name of the test
#'
#' @returns Repository file path as a character vector
prepare_local_test_repository <- function(test_name) {
  repository_path <- get_local_test_repository_path(test_name)
  if (file.exists(repository_path)){
    unlink(repository_path, recursive=TRUE)
  }
  dir.create(repository_path, recursive=TRUE)
  op <- options(amc.repository.path = repository_path)
  withr::defer_parent(options(op))
  repository_path
}

#' Repository path for cached test data.
#'
#' Data contained within this repository is cached between test runs.
#' Typically must be manually maintained, flushed after relevant changes.
get_cached_test_repository_path <- function() {
  file.path(tools::R_user_dir("amc_test", which = "data"), "cached")
}

#' Prepare a cached test repository to use with a test.
#'
#' This function prepares the cached test repository and sets it as the repository for the
#' scope of the test.
#'
#' Note that all data is shared amongst all tests. It is up to the tests to manage test independency.
#'
#' @param test_name Name of the test
#'
#' @returns Repository file path as a character vector
prepare_cached_test_repository <- function(test_name) {
  repository_path <- get_cached_test_repository_path()
  if (!file.exists(repository_path)){
    dir.create(repository_path, recursive=TRUE)
  }
  op <- options(amc.repository.path = repository_path)
  withr::defer_parent(options(op))
}
