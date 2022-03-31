
#' Get job details
#'
#' Return the settings used to create a job
#'
#' @param job A job ID string (UUID)
#' @param token An Encore API access token
#'
#' @return A object of class "encore_job"
#' @export
#'
#' @seealso \code{\link{get_jobs}}
#'
#' @examples
#' \dontrun{
#' job_id <- "123e4567-e89b-12d3-a456-426652340000"
#' get_job_details(job_id)
#' }

get_job_details <- function(job, token=get_current_access_token()) {
  if (("encore_job") %in% class(job)) {
    return(job)
  }
  clean <- function(x) {
    # rename
    from_name <- c("name", "type")
    to_name <- c("job_name", "model")
    for (i in seq_along(from_name)) {
      from <- from_name[i]
      to <- to_name[i]
      if(from %in% names(x$details)) {
        names(x$details)[names(x$details)==from] <- to
      }
    }
    # unlist
    for (v in c("covariates", "genopheno")) {
      if (v %in% names(x$details) && is.list(x$details[[v]])) {
        x$details[[v]] <- unlist(x$details[[v]])
      }
    }
    x
  }
  job_id <- get_job_id(job)
  if (length(job_id) != 1) {
    stop(paste("Expected 1 job ID, found", length(job_id)))
  }
  api_path <- paste0("/api/jobs/", job_id)
  resp <- eGET(api_path, token)
  if(httr::status_code(resp) != 200) {
    stop("Request Error", httr::content(resp))
  }
  result <- httr::content(resp)
  result <- clean(result)
  class(result) <- "encore_job"
  result
}


#' Get job list
#'
#' Return a list of all your current jobs
#'
#' @param search A character value used to search
#' the name, date, and ID fields for the jobs
#' @param limit Maximum number of records to return
#' @param token An Encore API access token
#'
#' @return A data.frame with class "encore_job_list"
#' @export
#'
#' @seealso \code{\link{get_job_details}}, \code{\link{download_job_output}},
#' \code{\link{get_phenotypes}}, \code{\link{get_genotypes}}
#'
#' @examples
#' \dontrun{
#' get_jobs()
#' get_jobs("saige-qt")
#' get_jobs(limit=1)
#' }
get_jobs <- function(search=NULL, limit=200, token=get_current_access_token()) {
  api_path <- "/api/jobs/"
  query <- list(limit=limit, q=search)
  resp <- eGET(api_path, token, query=query)
  if(httr::status_code(resp) != 200) {
    stop("Request Error", httr::content(resp))
  }
  result <- httr::content(resp)
  x <- make_data_frame(result)
  class(x) <- c("encore_job_list", class(x))
  x
}
