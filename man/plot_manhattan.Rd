% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manhattan.R
\name{plot_manhattan}
\alias{plot_manhattan}
\title{Create a Manhattan plot}
\usage{
plot_manhattan(
  job,
  palette = c("#7878ba", "#000042"),
  sig_line = 5e-08,
  scale_x = TRUE,
  scale_y = TRUE,
  scale_color = TRUE,
  theme = theme_manhattan(),
  token = get_current_access_token()
)
}
\arguments{
\item{job}{A job ID string (UUID)}

\item{palette}{A vector of color names for chromosomes}

\item{sig_line}{Where to draw the significance
line. Set to FALSE to turn line off}

\item{scale_x}{If TRUE, it will alter the x-axis
to show chromosome name. If FALSE, it will not add a scale
for the x axis. You
can also pass your own `scale_x_*` value here or you
can pass a function that can customize the default
scale values (see Details)}

\item{scale_y}{If TRUE, it will alter the y-axis
to match the default Encore display. If FALSE, it will not
a scale for the y axis.
You can also pass your own `scale_y_*` value here or you
can pass a function that can customize the default
scale values (see Details)}

\item{scale_color}{If TRUE, it will alter the color scale
to match the default Encore display. If FALSE, it will not
add a color scale.
You can also pass your own `scale_color_*` value here or you
can pass a function that can customize the default
scale values (see Details)}

\item{theme}{The default ggplot theme to apply. Set to NULL
if you do not wish to use the default ggplot2 theme}

\item{token}{An Encore API access token}
}
\value{
A ggplot2 object
}
\description{
Draw a Manhattan plot for a job using ggplot2
}
\details{
This function recreates the Manhattan plot visualization
from the Encore website using ggplot2. This makes
it easier to export/save plots for publications or
posters. This version of the plot is not interactive.

The object returns is a standard ggplot2 object. You
add additional elements such as annotations or themes
as you would most other ggplot2 objects.

in order to recreate the look of the web version
of the plot, several of the scales for the object
are pre-configured (`scale_x`, `scale_y`, and `scale_color`).
Because ggplot doesn't like it when you set a scale multiple
times on the same object, you have the option to turn off
the custom scales by setting these values to FALSE and
then can add your own scale to the object later.
If you prefer, you can also just pass a ggplot2 scale
object to this parameter and it will be used in place of the
default.
Alternatively, you can supply a function to these parameters
to change the defaults. This function must accept the raw
data as the first parameter, and then can capture parameters
for any of the default values it would like to change. The
function can then call which ever scale function with
whatever parameters you like.

Note that if you pass a job ID as input, each time you
call this function the data will be fetched from the
server. To prevent unnecessary network traffic, you can
download the data separately using `get_job_manhattan_data`
and can pass the result of that function directory
to this one.
}
\examples{
\dontrun{
job_id <- "123e4567-e89b-12d3-a456-426652340000"
plot_manhattan(job_id)
plot_manhattan(job_id,
  palette = c("#D94C1A","#57523E", "#F2BE24"),
  scale_x = function(data, ..., labels, guide) {
   # remove "chr" from chromosome names
   # and turn off dodging (by ignoring the altered guide= param)
   ggplot2::scale_x_continuous(..., labels=gsub("chr","", labels))
})
}
}
\seealso{
\code{\link{get_job_manhattan_data}}
}
