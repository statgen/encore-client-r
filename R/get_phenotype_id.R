#' Get phenotype ID
#'
#' A generic function to determine the phenotype ID for
#' a given object
#'
#' @param x A character value or an object that
#' contains phenotype data
#'
#' @return A character value with the phenotype ID(s)
#' @export
#'
#' @examples
#' \dontrun{
#' get_phenotype_id(get_phenotypes())
#' }
get_phenotype_id <- function (x) {
  UseMethod("get_phenotype_id", x)
}

#' @export
get_phenotype_id.encore_job <- function(x) x$details$phenotype
#' @export
get_phenotype_id.encore_job_list <- function(x) x$pheno_id
#' @export
get_phenotype_id.encore_pheno_list <- function(x) x$id


#' @export
get_phenotype_id.default <- function(x) {
  if(!is.character(x)) {
    stop(paste("Unable to determine phenotype ID from class:", class(x)))
  }
  if (any(bad_uuid <- !is_uuid(x))) {
    stop(paste0("Invalid phenotype ID. Expected valid UUID, found: ",
                paste0("'", x[bad_uuid], "'", collapse=", "), "."))
  }
  x
}
