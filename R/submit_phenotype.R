

#' Submit new phenotype file
#'
#' Upload phenotype data to Encore server
#'
#' @param x A data.frame or a path to a CSV file
#' @param ... Additional parameters for S3 methods
#' @param token An Encore API access token
#'
#' @return An "encore_phenotype" object
#' @export
#'
#' @examples
#' \dontrun{
#' sample <- data.frame(
#'   id = sprintf("NWD%06d", 1:100),
#'   value = rnorm(100),
#'   group = sample(c("X","Y"), 100, replace=TRUE)
#' )
#' submit_phenotype(sample)
#' }
submit_phenotype <- function(x, ..., token = get_current_access_token()) {
  UseMethod("submit_phenotype", x)
}

#' @rdname submit_phenotype
#' @method submit_phenotype default
#' @importFrom utils write.csv
#' @export
submit_phenotype.default <- function(x, ..., token = get_current_access_token()) {
  if(!is.character(x)) {
    stop(paste("Unable to submit object of class:", class(x),
               " - expecting character or data.frame"))
  }
  stopifnot(file.exists(x))
  api_url <- "/api/phenos"
  resp <- ePOST(api_url,
        body = list(pheno_file = httr::upload_file(x)),
        token = token)
  if(httr::status_code(resp) != 200) {
    stop("Request Error", httr::content(resp))
  }
  httr::content(resp)$pheno_id
}

#' @param filename Override the default CSV name
#' @rdname submit_phenotype
#' @method submit_phenotype data.frame
#' @export
submit_phenotype.data.frame <- function(x, filename = NULL, ..., token = get_current_access_token()) {
  if (is.null(filename)) {
    if (is.name(substitute(x))) {
      filename <- paste0(deparse1(substitute(x)), ".csv")
    } else {
      filename <- paste0("pheno-", format(Sys.time(), "%Y-%m-%d-%H:%M:%S"), ".csv")
    }
  }
  filepath <- file.path(tempdir(), filename)
  utils::write.csv(x, file = filepath, row.names = FALSE)
  on.exit(unlink(filepath))
  submit_phenotype(filepath, token = token)
}
