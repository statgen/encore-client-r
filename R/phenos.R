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

upload_phenotype <- function(pheno_path, token=get_current_access_token()) {
  stopifnot(file.exists(pheno_path))
  api_url <- paste0(get_encore_server(), "/api/phenos")
  resp <- httr::POST(api_url,
              httr::add_headers("Authorization"=paste("Bearer", token)),
              body = list(pheno_file = httr::upload_file(pheno_path)))
  if(httr::status_code(resp) != 200) {
    stop("Request Error", httr::content(resp))
  }
  httr::content(resp)$pheno_id
}
