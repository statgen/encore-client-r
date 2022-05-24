
#' Get Manhattan plot data
#'
#' This function fetches Manhattan plot data from API Server
#'
#' This function will return the data used to draw
#' the Manhattan plot on the Encore website. Note
#' that the data has been summarized on the server
#' and binned in order to reduce the number of points
#' to be drawn.
#'
#' @param job A job ID string (UUID)
#' @param token An Encore API access token
#'
#' @seealso \code{\link{plot_manhattan}}
#' @return This will return an object of type "encore_manhattan_data"
#' that can be used with the `plot_manhattan` function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' job_id <- "123e4567-e89b-12d3-a456-426652340000"
#' data <- get_job_manhattan_data(job_id)
#' plot_manhattan(data)
#' }
get_job_manhattan_data <- function(job, token=get_current_access_token()) {
  job_id <- get_job_id(job)
  stopifnot(length(job_id)==1)
  api_path <- paste0("/api/jobs/", job_id, "/plots/manhattan")
  resp <- eGET(api_path, token)
  if(httr::status_code(resp) != 200) {
    stop("Request Error", httr::content(resp))
  }
  data <- httr::content(resp)

  unbinned <- type.convert(rbind_fill(data$unbinned_variants), as.is=TRUE)

  bin_prop <- do.call("rbind.data.frame", lapply(data$variant_bins, function(x) {
    data.frame(chrom=x$chrom, pos=x$pos,
               n_points = length(x$neglog10_pvals),
               n_lines = length(x$neglog10_pval_extents))
  }))
  chroms <- unique(bin_prop$chrom)
  unbinned$chrom <- factor(unbinned$chrom, levels=chroms)
  bin_prop$chrom <- factor(bin_prop$chrom, levels=chroms)
  chr_ranges <- Map(range,
    tapply(unbinned$pos, unbinned$chrom, range),
    tapply(bin_prop$pos, bin_prop$chrom, range)
  )
  chr_start <- sapply(chr_ranges, min)/1e6
  chr_len <- (sapply(chr_ranges, max) - chr_start)/1e6
  chr_offset <- c(0, head(cumsum(chr_len+5),-1))
  chr_centers <- chr_len/2 + chr_offset
  chr_breaks <- chr_offset[-1]
  names(chr_offset) <- names(chr_len)
  gen_pos <- function(chrom, pos) {
    pos/1e6 - chr_start[chrom] + chr_offset[chrom]
  }

  set_col_names <- function(x, n) {colnames(x) <- n; x}
  drop_row_names <- function(x) {rownames(x) <- NULL; x}
  binned_points <- cbind(
    drop_row_names(bin_prop[ rep(seq.int(nrow(bin_prop)), bin_prop$n_points), c("chrom","pos")]),
    set_col_names(do.call("rbind", lapply(data$variant_bins, function(x) {
    vals <- unlist(x$neglog10_pvals)
    if (length(vals)) matrix(vals, ncol=1, byrow = TRUE)
    })),c("y"))
  )

  binned_lines <- cbind(
    drop_row_names(bin_prop[ rep(seq.int(nrow(bin_prop)), bin_prop$n_lines), c("chrom","pos")]),
    set_col_names(do.call("rbind", lapply(data$variant_bins, function(x) {
    vals <- unlist(x$neglog10_pval_extents)
    if (length(vals)) matrix(vals, ncol=2, byrow = TRUE)
  })), c("low","high")))

  unbinned$genpos <- with(unbinned, gen_pos(chrom, pos))
  binned_points$genpos <- with(binned_points, gen_pos(chrom, pos))
  binned_lines$genpos <- with(binned_lines, gen_pos(chrom, pos))

  x <- list(unbinned = unbinned,
       binned_points=binned_points, binned_lines=binned_lines,
       chr_centers=chr_centers, chr_breaks = chr_breaks)
  attr(x, "job_id") <- job_id
  class(x) <- "encore_manhattan_data"
  x
}


#' Create a Manhattan plot
#'
#' Draw a Manhattan plot for a job using ggplot2
#'
#' This function recreates the Manhattan plot visualization
#' from the Encore website using ggplot2. This makes
#' it easier to export/save plots for publications or
#' posters. This version of the plot is not interactive.
#'
#' The object returns is a standard ggplot2 object. You
#' add additional elements such as annotations or themes
#' as you would most other ggplot2 objects.
#'
#' in order to recreate the look of the web version
#' of the plot, several of the scales for the object
#' are pre-configured (`scale_x`, `scale_y`, and `scale_color`).
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
#' download the data separately using `get_job_manhattan_data`
#' and can pass the result of that function directory
#' to this one.
#'
#'
#' @param job A job ID string (UUID)
#' @param palette A vector of color names for chromosomes
#' @param sig_line Where to draw the significance
#' line. Set to FALSE to turn line off
#' @param scale_x If TRUE, it will alter the x-axis
#' to show chromosome name. If FALSE, it will not add a scale
#' for the x axis. You
#' can also pass your own `scale_x_*` value here or you
#' can pass a function that can customize the default
#' scale values (see Details)
#' @param scale_y If TRUE, it will alter the y-axis
#' to match the default Encore display. If FALSE, it will not
#' a scale for the y axis.
#' You can also pass your own `scale_y_*` value here or you
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
#'
#' @seealso \code{\link{get_job_manhattan_data}}
#'
#' @examples
#' \dontrun{
#' job_id <- "123e4567-e89b-12d3-a456-426652340000"
#' plot_manhattan(job_id)
#' plot_manhattan(job_id,
#'   palette = c("#D94C1A","#57523E", "#F2BE24"),
#'   scale_x = function(data, ..., labels, guide) {
#'    # remove "chr" from chromosome names
#'    # and turn off dodging (by ignoring the altered guide= param)
#'    ggplot2::scale_x_continuous(..., labels=gsub("chr","", labels))
#' })
#' }
plot_manhattan <- function(job,
  palette = c("#7878ba", "#000042"),
  sig_line = 5e-8,
  scale_x = TRUE, scale_y = TRUE, scale_color = TRUE,
  theme = theme_manhattan(),
  token = get_current_access_token()) {


  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for plotting. Please install it.",
         call. = FALSE)
  }

  data <- if (inherits(job, "encore_manhattan_data")) {
    job
  } else {
    job_id <- get_job_id(job)
    if(length(job_id)!=1) {
      stop(paste("Expected 1 job, but", length(job_id), "found."))
    }
    get_job_manhattan_data(job_id, token=token)
  }
  ggplot2::ggplot(data$unbinned) +
    {
      if (is.numeric(sig_line)) {
        stopifnot(sig_line > 0)
      } else if (is.logical(sig_line)) {
        sig_line <- ifelse(sig_line, 5e-8, 0)
      } else {
        stop(paste("Invalid sig_line, expected numeric, found", class(sig_line)))
      }
      if (sig_line > 0) {
        if (sig_line < 1) {
          sig_line <- -log10(sig_line)
        }
        ggplot2::geom_hline(yintercept = sig_line, linetype=2)
      }
    } +
    ggplot2::geom_point(ggplot2::aes(.data$genpos, -log10(.data$pval), color=.data$chrom)) +
    ggplot2::geom_point(ggplot2::aes(.data$genpos, .data$y, color=.data$chrom), data=data$binned_points) +
    ggplot2::geom_segment(ggplot2::aes(.data$genpos, .data$low, xend=.data$genpos, yend=.data$high, color=.data$chrom), data=data$binned_lines, size=2, lineend = "round") +
    {if ((is.logical(scale_color) && scale_color) || is.function(scale_color)) {
      breaks <- levels(data$unbinned$chrom)
      if(length(palette) < length(breaks)) {
        palette <- rep(palette, length.out = nlevels(data$unbinned$chrom))
      }
      fun <- if (is.function(scale_color)) scale_color else function(data, ...) ggplot2::scale_color_manual(...)
      fun(data, guide = "none", breaks=breaks, values = palette)
    } else if (is.logical(scale_color) && !scale_color) {
      NULL
    } else {
      scale_color
    }} +
    {if ((is.logical(scale_y) && scale_y) || is.function(scale_y)) {
      limits <- range(c(0, range(-log10(data$unbinned$pval))+.1, sig_line + .5))
      expand <- ggplot2::expansion(0,0)
      fun <- if (is.function(scale_y)) scale_y else function(data, ...) ggplot2::scale_y_continuous(...)
      fun(data, limits=limits, expand=expand)
    } else if (is.logical(scale_y) && !scale_y) {
      NULL
    } else {
      scale_y
    }} +
    {if ((is.logical(scale_x) && scale_x) || is.function(scale_x)) {
      breaks <- data$chr_centers
      minor_breaks <- data$chr_breaks
      labels <- names(data$chr_centers)
      guide <- ggplot2::guide_axis(n.dodge = 2)
      expand <- ggplot2::expansion(0,20)
      fun <- if (is.function(scale_x)) scale_x else function(data, ...) ggplot2::scale_x_continuous(...)
      fun(data, breaks=breaks, minor_breaks=minor_breaks, labels=labels, guide=guide, expand=expand)
    } else if (is.logical(scale_x) && !scale_x) {
      NULL
    } else {
      scale_x
    }} +
    ggplot2::labs(x="") +
    theme
}

#' Manhattan plot theme
#'
#' Default ggplot2 theme for Manhattan plot
#'
#' This uses the black and white ggplot theme and
#' turns off the grid lines for the chromosome
#' centers (major breaks)
#'
#' @param ... Values passed along to the ggplot::theme() function
#' @param panel.grid.major Styling chromosome center grid marks
#'
#' @return A ggplot2 object
#' @export

theme_manhattan <- function(..., panel.grid.major = ggplot2::element_blank()) {
  ggplot2::theme_bw(...) +
  ggplot2::theme(panel.grid.major = panel.grid.major)
}
