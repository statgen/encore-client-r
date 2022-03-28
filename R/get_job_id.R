#' Get job ID
#'
#' A generic function to determine the job ID for
#' a given object
#'
#' @param job A character value or an object that
#' contains job data
#'
#' @return A character value with the job ID(s)
#' @export
#'
#' @examples
#' \dontrun{
#' get_job_id(get_jobs())
#' }
get_job_id <- function (job) {
  UseMethod("get_job_id", job)
}

#' @export
get_job_id.encore_job <- function(job) job$job_id
#' @export
get_job_id.encore_job_list <- function(job) job$id
#' @export
get_job_id.encore_qq_data <- function(job) attr(job, "job_id")
#' @export
get_job_id.encore_manhattan_data <- function(job) attr(job, "job_id")

#' @export
get_job_id.default <- function(job) {
  if(!is.character(job)) {
    stop(paste("Unable to determine job ID from class:", class(job)))
  }
  if (any(bad_uuid <- !is_uuid(job))) {
    stop(paste0("Invalid job ID. Expected valid UUID, found: ",
                paste0("'", job[bad_uuid], "'", collapse=", "), "."))
  }
  job
}
