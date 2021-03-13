.state <- new.env(parent = emptyenv())

get_current_access_token <- function() {
  if (exists("token", where = .state) ) {
    .state$token
  } else {
    envar <- Sys.getenv("ENCORE_API_TOKEN")
    if (nchar(envar)>0) {
      .state$token <- envar
      envar
    } else {
      stop(paste("There is no current access token currently set.",
               "Please see ?set_access_token for more info"))
    }
  }
}

get_encore_server <- function() {
  if (exists("server", where = .state) ) {
    .state$server
  } else {
    "https://encore.sph.umich.edu"
  }
}

#' Set API access token
#'
#' Set the default Encore API access token
#'
#' In order to use the Encore API, you must first
#' log in and generate an API access token on the
#' website itself. You'll receive a long string of
#' letters and numbers (a JSON web token). Most
#' functions will allow you to pass a token as a
#' parameter, but by using this function, you set
#' the default access token you so don't have to
#' pass it as a parameter to every function you call.
#'
#' It's important to keep your access token secure.
#' For some advice on ways to store the value without
#' typing it in the console, see `vignette("secrets", package="httr")`
#'
#' @param token A character value containing Encore API access token.
#' If you are using RStudio you can leave this value blank and a window
#' will open to ask for your token. The prevents the token from being stored
#' in your history file to help keep it more secure.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # This will open a prompt if using RStudio to enter your token
#' set_access_token()
#'
#' # Note your actual token will be much longer
#' # (This example token is not valid)
#' set_access_token("eyJhbGciOi9.eyJuYW1lIjoiRW5.e30XBiuDHBYDxSLT")
#' }
set_access_token <- function(token) {
  if(missing(token)) {
    if(requireNamespace("rstudioapi", quietly = TRUE)) {
      token <- rstudioapi::askForPassword("Enter Encore API access token")
    }
  }
  .state$token <- token
}

set_encore_server <- function(server) {
  .state$server <- server
}
