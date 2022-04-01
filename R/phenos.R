
#' Get phenotype details
#'
#' Returns metadata for phenotype file
#'
#' @param pheno A phenotype ID string (UUID)
#' @param token An Encore API access token
#'
#' @return A object of class "encore_phenotype"
#' @export
#'
#' @seealso \code{\link{get_phenotypes}}
#'
#' @examples
#' \dontrun{
#' pheno_id <- "123e4567-e89b-12d3-a456-426652340000"
#' get_phenotype_details(pheno_id)
#' }
get_phenotype_details <- function(pheno, token = get_current_access_token()) {
  if (("encore_phenotype") %in% class(pheno)) {
    return(pheno)
  }
  pheno_id <- get_phenotype_id(pheno)
  if (length(pheno_id) != 1) {
    stop(paste("Expected 1 phenotype ID, found", length(pheno_id)))
  }
  api_path <- paste0("/api/phenos/", pheno_id)
  resp <- eGET(api_path, token)
  if(httr::status_code(resp) != 200) {
    stop("Request Error", httr::content(resp))
  }
  result <- httr::content(resp)
  class(result) <- "encore_phenotype"
  result
}

#' Get phenotype list
#'
#' Return a list of all your phenotype files
#'
#' @param search A character value used to filter
#' your phenotype list
#' @param limit Maximum number of records to return
#' @param token An Encore API access token
#'
#' @return A data.frame with class "encore_pheno_list"
#' @export
#'
#' @examples
#' \dontrun{
#' get_phenotypes()
#' get_phenotypes("my favorite file")
#' get_phenotypes(limit=1)
#' }
get_phenotypes <- function(search=NULL, limit=200, token=get_current_access_token()) {
  api_path <- "/api/phenos/"
  query <- list(limit=limit, q=search)
  resp <- eGET(api_path, token, query=query)
  if(httr::status_code(resp) != 200) {
    stop("Request Error", httr::content(resp))
  }
  result <- httr::content(resp)
  x <- make_data_frame(result)
  class(x) <- c("encore_pheno_list", class(x))
  x
}
