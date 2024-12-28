#' List of valid values for dataset status
#'
#' @export
#'
#' @examples
#' amc_dataset_status$ready
amc_dataset_status <- list(
  # Dataset is missing
  missing = "missing",
  # Dataset is being downloaded
  downloading = "downloading",
  # Dataset download has failed
  download_failed = "download_failed",
  # Dataset ready
  ready = "ready"
)

get_amc_dataset_state_path <- function(dataset) {
  state_filename <- paste0(dataset$dataset_code, "-state.rds")
  state_filepath <- get_amc_dataset_path(dataset, state_filename)
  state_filepath
}

new_amc_dataset_state <- function(dataset, status, prepare_date = NULL, fail_condition = NULL) {
  append(
    dataset,
    list(
      status = status,
      prepare_date = prepare_date,
      fail_condition = fail_condition
    )
  )
}

#' Read AMC dataset state
#'
#' @param dataset Dataset to get state of
#'
#' @returns List that identifues
#' @export
#'
#' @examples
#' \donttest{
#'   read_amc_dataset_state(giss_ta_zonal_yearly_dataset)
#' }
#'
read_amc_dataset_state <- function(dataset) {
  state_filepath <- get_amc_dataset_state_path(dataset)
  if (!file.exists(state_filepath)) {
    return(new_amc_dataset_state(dataset, amc_dataset_status$missing))
  }
  state <- readRDS(state_filepath)
  return(state)
}

#' Save AMC dataset state
#'
#' @param dataset The dataset we are saving the state of
#' @param status Dataset status, Should be one of the members of the amc_dataset_status list
#' @param prepare_date Optional date the dataset was prepared
#' @param fail_condition Condition that caused dataset preparation to fail
#'
#' @returns Saved state
#' @export
#'
#' @examples
#' \donttest{
#'   save_amc_dataset_state(giss_ta_zonal_yearly_dataset, amc_dataset_status$ready)
#' }
save_amc_dataset_state <- function(dataset, status, prepare_date = NULL, fail_condition = NULL) {
  prepare_amc_dataset_repository(dataset)
  state_filepath <- get_amc_dataset_state_path(dataset)
  state <- new_amc_dataset_state(dataset, status, prepare_date = prepare_date, fail_condition = fail_condition)
  saveRDS(state, state_filepath)
  return(state)
}
