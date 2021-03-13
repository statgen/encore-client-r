#' Get QQ plot data
#'
#' This function fetches QQ plot data from API Server
#'
#' This function will return the data used to draw
#' the p-value QQ plot on the Encore website. Note
#' that the data has been summarized on the server
#' and binned in order to reduce the number of points
#' to be drawn.
#'
#'
#' @param job A job ID string (UUID)
#' @param index If multiple QQ plots are returned, the
#'   index for the one you want to use
#' @param token An Encore API access token
#'
#' @seealso \code{\link{plot_qq}}
#' @return This will return an object of type "encore_qq_data"
#' that can be used with the `plot_qq` function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' job_id <- "123e4567-e89b-12d3-a456-426652340000"
#' data <- get_job_manhattan_data(job_id)
#' plot_manhattan(data)
#' }

get_job_qq_data <- function(job, index, token=get_current_access_token()) {
  job_id <- get_job_id(job)
  stopifnot(length(job_id)==1)
  api_path <- paste0("/api/jobs/", job_id, "/plots/qq")
  resp <- eGET(api_path, token)
  if(httr::status_code(resp) != 200) {
    stop("Request Error", httr::content(resp))
  }
  clean_layer_data <- function(x) {
    binned <- do.call("rbind.data.frame", x$variant_bins)
    names(binned) <- c("expected","observed")
    unbinned <- type.convert(do.call("rbind.data.frame", lapply(x$unbinned_variants, function(z) c(list(expected=z[[1]], observed=z[[2]]), z[[3]]))), as.is=TRUE)
    extra_cols <- setdiff(names(unbinned), names(binned))
    binned[, extra_cols] <- rep(list(NA), length(extra_cols))
    binned$binned <- TRUE
    unbinned$binned <- FALSE
    data <- rbind(unbinned, binned)
    ci <- do.call("rbind.data.frame", x$conf_int)
    names(ci) <- c("expected","high","low")
    attr(data, "gc") <- x$gc[["50"]]
    attr(data, "count") <- x$count
    attr(data, "confidence_interval") <- ci
    if ("maf_range" %in% names(x)) {
      attr(data, "maf_range") <- unlist(x$maf_range)
    }
    data
  }
  if (missing(index)) {
    if (length(resp$data)>1) {
      warning(strwrap(paste0(c("Returning data for plot 1 of ",
        length(resp$data), ". Suppress this message by supplying ",
        "index=1 to function")), initial="", prefix=" "))
    }
    index <- 1
  }
  resp <- httr::content(resp)
  if (index >0 && index<=length(resp$data)) {
    data <- resp$data[[index]]
  } else {
    stop(paste("Invalid index value:", index,
              "(must be between 1 and", length(data)))
  }
  if (length(data$layers) > 1) {
    parts <- lapply(data$layers, clean_layer_data)
    level_names <- sapply(data$layers, `[[`, "level")
    x <- do.call("rbind", Map(function(d, l) {
      d$group=l; d
    }, parts, level_names))
    layer_info <- lapply(parts, function(x) {
      attr <- attributes(x)
      attr[intersect(names(attr), c("gc","confidence_interval",
                                 "count","maf_range"))]
    })
    attr(x, "gc") <- NULL
    attr(x, "maf_range") <- NULL
    x$group <- factor(x$group, levels=level_names)
    names(layer_info) <- level_names
    attr(x, "group_info") <- layer_info
    attr(x, "count") <- sum(sapply(layer_info, function(x) {
      if (!is.null(x$count)) x$count else NA
    }), na.rm=TRUE)
  } else if (length(data$layers) == 1) {
    x <- clean_layer_data(data$layers[[1]])
  }
  attr(x, "job_id") <- job_id
  class(x) <- c("encore_qq_data", class(x))
  x
}

#' Create a QQ plot
#'
#' Create a QQ plot using the p-values from the job
#'
#' This function recreates the QQ plot visualization
#' from the Encore website using ggplot2. This makes
#' it easier to export/save plots for publications or
#' posters. This version of the plot is not interactive.
#'
#' The QQ plot will draw the observed -log10 p-values
#' against the expected distribution of p-values under
#' the null assumptions that the p-values are uniformly
#' distributed between 0 and 1.

#' The object returns is a standard ggplot2 object. You
#' add additional elements such as annotations or themes
#' as you would most other ggplot2 objects.
#'
#' In order to recreate the look of the web version
#' of the plot, several of the scales for the object
#' are pre-configured (`scale_color`, and `coord`).
#' Because ggplot doesn't like it when you set a scale multiple
#' times on the same object, you have the option to turn off
#' the custom scales by setting these values to FALSE and
#' then can add your own scale to the object later.
#' If you prefer, you can also just pass a ggplot2 scale
#' object to this parameter and it will be used in place of the
#' default.
#' Alternatively, you can supply a function to these parameters
#' to change the defaults. This function must accept the raw
#' data as the first parameter, and then can capture parameters
#' for any of the default values it would like to change. The
#' function can then call which ever scale function with
#' whatever parameters you like.
#'
#' Note that if you pass a job ID as input, each time you
#' call this function the data will be fetched from the
#' server. To prevent unnecessary network traffic, you can
#' download the data separately using `get_job_qq_data`
#' and can pass the result of that function directory
#' to this one.
#'
#'
#' @param job A job ID string (UUID)
#' @param conf_int A logical value indicating if
#' the confidence interval should be drawn
#' @param coord  If TRUE, it will alter the coordiantes
#' to coord_fixed to scale the x and y axis the same.
#' If FALSE, it will not set the coordinates.
#' You can also pass your own `coord_*` value here or you
#' can pass a function that can customize the default
#' scale values (see Details)
#' @param scale_color If TRUE, it will alter the color scale
#' to match the default Encore display. If FALSE, it will not
#' add a color scale.
#' You can also pass your own `scale_color_*` value here or you
#' can pass a function that can customize the default
#' scale values (see Details)
#' @param theme The default ggplot theme to apply. Set to NULL
#' if you do not wish to use the default ggplot2 theme
#' @param token An Encore API access token
#'
#' @return A ggplot2 object
#' @export
#' @seealso \code{\link{get_job_qq_data}}
#'
#' @examples
#' \dontrun{
#' job_id <- "123e4567-e89b-12d3-a456-426652340000"
#' plot_qq(job_id)
#' }
plot_qq <- function(job,
  conf_int = TRUE,
  coord = TRUE,
  scale_color = TRUE,
  theme = ggplot2::theme_bw(),
  token = get_current_access_token()) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for plotting. Please install it.",
         call. = FALSE)
  }

  data <- if (inherits(job, "encore_qq_data")) {
    job
  } else {
    job_id <- get_job_id(job)
    if(length(job_id)!=1) {
      stop(paste("Expected 1 job, but", length(job_id), "found."))
    }
    get_job_qq_data(job_id, token=token)
  }
  limits <- range(c(0, range(data$expected), range(data$observed)))
  has_groups <- "group" %in% names(data)
  ggplot2::ggplot(data) +
    {if (conf_int) {
      cidata <- attr(data, "confidence_interval")
      ggplot2::geom_ribbon(ggplot2::aes(x=.data$expected, ymin=.data$low, ymax=.data$high), data=cidata, alpha=.2)
      } else {
        NULL
    }} +
    { point_aes <- if (has_groups) {
        ggplot2::aes(.data$expected, .data$observed, color=.data$group)
      } else {
        ggplot2::aes(.data$expected, .data$observed)
      }
      ggplot2::geom_point(point_aes)
    } +
    {if ((is.logical(scale_color) && scale_color) || is.function(scale_color)) {
      fun <- if (is.function(scale_color)) scale_color else function(data, ...) ggplot2::scale_color_manual(...)
      values = c("#E66101","#FDB863","#B2ABD2", "#5E3C99")
      labels = sapply(attr(data, "group_info"), function(x) {
        paste(signif(x$maf_range[1],3), "<= MAF <", signif(x$maf_range[2],3))
      })
      fun(data, breaks = names(attr(data, "group_info")), values = values, labels=labels)
    } else if (is.logical(scale_color) && !scale_color) {
      NULL
    } else {
      scale_color
    }} +
    {if (is.logical(coord) && coord) {
      ggplot2::coord_fixed()
    } else if (is.logical(coord) && !coord) {
      NULL
    } else {
      coord
    }} +
    theme
}
