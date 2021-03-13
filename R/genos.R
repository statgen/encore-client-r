#' Get genotype list
#'
#' Return a list of all available genotype freezes
#'
#' @param search A character value used to filter
#' the genotype list
#' @param limit Maximum number of records to return
#' @param token An Encore API access token
#'
#' @return A data.frame with class "encore_geno_list"
#' @export
#'
#' @examples
#' \dontrun{
#' get_genotypes()
#' get_genotypes("Freeze 5")
#' get_genotypes(limit=1)
#' }
get_genotypes <- function(search=NULL, limit=200, token=get_current_access_token()) {
  api_path <- "/api/genos/"
  query <- list(limit=limit, q=search)
  resp <- eGET(api_path, token, query=query)
  if(httr::status_code(resp) != 200) {
    stop("Request Error", httr::content(resp))
  }
  result <- httr::content(resp)
  x <- make_data_frame(result)
  class(x) <- c("encore_geno_list", class(x))
  x
}
