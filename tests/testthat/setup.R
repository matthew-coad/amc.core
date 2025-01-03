
# Give tests their own repository by
test_repository_path <- file.path(tools::R_user_dir("amc_test", which = "data"))
default_options <- options(amc.core.repository.path = NULL)
withr::defer(options(default_options), teardown_env())
