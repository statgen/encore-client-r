#' Create new job request from existing
#'
#' This function helps to create a new job
#' request that you can pass to `submit_job`.
#' The request itself is just a named list with
#' values that would match what you enter on the
#' Encore website
#'
#' You can pass in an existing job to copy all the
#' existing parameters and just change the ones
#' you want to be different.
#'
#' Alternatively, you can pass in named parameters
#' to override existing values or simply set new ones
#'
#' @param job A character value with a job ID
#' or an object that contains job data
#' @param ... Additional properties to set for new job
#' @param token An Encore API access token
#'
#' @return A list containing parameters used
#' to create job
#' @export
#'
#' @seealso \code{\link{submit_job}}
#'
#' @importFrom utils modifyList
#'
#' @examples
#' \dontrun{
#' new_job <- as_new_job(get_jobs(limit=1))
#' new_job$name <- paste(new_job$name, "Rerun")
#' submit_job(new_job)
#' }
as_new_job <- function(job = NULL, ..., token = get_current_access_token()) {
  params <- list()
  if (!is.null(job)) {
    x <- get_job_details(job, token = token)
    params <- x$details
    todrop <- c("user_id", "variant_filter_desc",
                "pipeline_version", "model_desc")
    params <- params[-which(names(params) %in% todrop)]
  }
  modifyList(params, list(...))
}


#' Submit new job request
#'
#' @param x A named list of job parameters
#' @param token An Encore API access token
#'
#' @return A object of class "encore_job"
#' @export
#'
#' @seealso \code{\link{as_new_job}}
#'
#' @examples
#' \dontrun{
#' new_job <- as_new_job(get_jobs(limit=1))
#' new_job$name <- paste(new_job$name, "Rerun")
#' submit_job(new_job)
#' }
submit_job <- function(x, token = get_current_access_token()) {
  resp <- ePOST("/api/jobs", token = token, body = x)
  resp_code <- httr::status_code(resp)
  if(resp_code == 303) {
    result <- httr::content(resp)
    if (is.null(result$duplicates) || is.null(result$duplicates[[1]]$id)) {
      stop("Duplicate job -- cannot extract existing ID")
    }
    message("Duplicate Job request")
    new_id <- result$duplicates[[1]]$id
  } else if (resp_code != 200) {
    stop("Request Error: ", httr::content(resp))
  } else {
    result <- httr::content(resp)
    new_id <- result$id
    if (is.null(new_id)) {
      stop("Could not find new job ID")
    }
  }
  get_job_details(new_id, token = token)
}
