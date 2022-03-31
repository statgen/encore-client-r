is_uuid <- function(x) {
  grepl("^[0-9a-f]{8}-[0-9a-f]{4}-[0-5][0-9a-f]{3}-[089ab][0-9a-f]{3}-[0-9a-f]{12}$", x, ignore.case=TRUE)
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

flatform <- function(x) {
  # A form can only have one value per name, so take
  # any values that contain vectors length >1 and
  # split them up
  # list(x=1:2, y="a") becomes list(x=1, x=2, y="a")
  lens <- lengths(x)
  if (all(lens<=1)) return(x);
  z <- do.call("c", lapply(x, as.list))
  names(z) <- rep(names(x), lens)
  z
}

ePOST <- function(path, token, body=NULL, encode="flatform", ...) {
  url <- paste0(get_encore_server(), path)
  if (encode == "flatform") {
    body <- flatform(body)
    encode <- "form"
  }
  resp <- httr::POST(url,
                     httr::add_headers("Authorization"=paste("Bearer", token)),
                     body = body,
                     encode = encode,
                     ...)
  if(httr::status_code(resp) == 401) {
    stop(paste("Unauthorized request.",
               "Try refreshing your API access token. See ?set_access_token"))
  }
  resp
}
