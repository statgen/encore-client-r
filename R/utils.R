is_uuid <- function(x) {
  grepl("^[0-9a-f]{8}-[0-9a-f]{4}-[0-5][0-9a-f]{3}-[089ab][0-9a-f]{3}-[0-9a-f]{12}$", x, ignore.case=TRUE)
}

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

make_data_frame <- function(data, header=NULL) {
  if (all(c("header","data") %in% names(data))) {
    header <- data$header
    data <- data$data
  }
  nulltona <- function(x) {x[sapply(x, is.null)] <- NA; x}
  x <- do.call("rbind.data.frame", c(lapply(data, nulltona), make.row.names=FALSE))
  date_cols <- grep("_date$", names(x))
  if (length(date_cols)>1) {
    x[, date_cols] <- lapply(x[,date_cols, drop=FALSE], as.POSIXct, format="%Y-%m-%d %H:%M:%S")
  }
  if (!is.null(header) && !is.null(header[["next"]])) {
    attr(x, "next_page") <- header[["next"]]
  }
  x
}

## Helper functions for paged results
has_next <- function(x) {
  next_page <- attr(x, "next_page")
  !is.null(next_page)
}

next_page <- function(x, token=get_current_access_token()) {
  if (!has_next(x)) {
    warning("No additional page information found")
    return(NULL)
  }
  path <- attr(x, "next_page")
  page <- make_data_frame(httr::content(eGET(path, token=token)))
  class(page) <- class(x)
  page
}

# Wrappers around httr functions to add API access
# token and the server URL
eGET <- function(path, token, ...) {
  url <- paste0(get_encore_server(), path)
  resp <- httr::GET(url,
            httr::add_headers("Authorization"=paste("Bearer", token)),
            ...)
  if(httr::status_code(resp) == 401) {
    stop(paste("Unauthorized request.",
                "Try refreshing your API access token. See ?set_access_token"))
  }
  resp
}
eHEAD <- function(path, token, ...) {
  url <- paste0(get_encore_server(), path)
  resp <- httr::HEAD(url,
            httr::add_headers("Authorization"=paste("Bearer", token)),
            ...)
  if(httr::status_code(resp) == 401) {
    stop(paste("Unauthorized request.",
               "Try refreshing your API access token. See ?set_access_token"))
  }
  resp
}
