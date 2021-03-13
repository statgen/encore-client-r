#' Download job output
#'
#' Download raw results file from Encore server for given jobs
#'
#' @param job A character vector of job IDs (UUIDs)
#' @param output_name Optional file name for download. Will
#' default to the name used on the server
#' @param verbose Show file names and progress of downloads if TRUE
#' @param ... Optional parameters passed through to `httr::write_disk`
#' @param token An Encore API access token
#'
#' @export
#'
#' @return Invisibly returns the list of files created
#'
#' @examples
#' \dontrun{
#' # Download file to current directory
#' job_id <- "123e4567-e89b-12d3-a456-426652340000"
#' download_job_output(job_id)
#' # Download results from ALL jobs
#' download_job_output(get_jobs())
#' }
download_job_output <- function(job, output_name=NULL, verbose=TRUE, ... , token=get_current_access_token()) {
  job_id <- get_job_id(job)
  api_path <- paste0("/jobs/", job_id, "/output")
  query_output_name <- function(path) {
    temp <- eHEAD(path, token)
    sub(".*filename=", "", httr::headers(temp)$`content-disposition`)
  }
  if (!is.null(output_name)) {
    if(length(api_path) != length(output_name))
      stop(paste0("Number of jobs (", length(job_id),
             ") does not match number of output names (",
             length(output_name), ")"))
  } else {
    output_name <- rep(NA, length(job_id))
  }
  for(i in seq_along(api_path)) {
    path <- api_path[i]
    file_name <- output_name[i]
    if(is.na(file_name)) {
      file_name <- query_output_name(path)
      output_name[i] <- file_name
    }
    if (verbose) {
      message(paste(job_id[i], "=>", file_name))
    }
    eGET(path, token, httr::write_disk(file_name, ...), if(verbose) httr::progress())
  }
  invisible(output_name)
}
