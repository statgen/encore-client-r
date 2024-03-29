% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qq.R
\name{plot_qq}
\alias{plot_qq}
\title{Create a QQ plot}
\usage{
plot_qq(
  job,
  conf_int = TRUE,
  coord = TRUE,
  scale_color = TRUE,
  theme = ggplot2::theme_bw(),
  token = get_current_access_token()
)
}
\arguments{
\item{job}{A job ID string (UUID)}

\item{conf_int}{A logical value indicating if
the confidence interval should be drawn}

\item{coord}{If TRUE, it will alter the coordiantes
to coord_fixed to scale the x and y axis the same.
If FALSE, it will not set the coordinates.
You can also pass your own `coord_*` value here or you
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
Create a QQ plot using the p-values from the job
}
\details{
This function recreates the QQ plot visualization
from the Encore website using ggplot2. This makes
it easier to export/save plots for publications or
posters. This version of the plot is not interactive.

The QQ plot will draw the observed -log10 p-values
against the expected distribution of p-values under
the null assumptions that the p-values are uniformly
distributed between 0 and 1.
The object returns is a standard ggplot2 object. You
add additional elements such as annotations or themes
as you would most other ggplot2 objects.

In order to recreate the look of the web version
of the plot, several of the scales for the object
are pre-configured (`scale_color`, and `coord`).
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
download the data separately using `get_job_qq_data`
and can pass the result of that function directory
to this one.
}
\examples{
\dontrun{
job_id <- "123e4567-e89b-12d3-a456-426652340000"
plot_qq(job_id)
}
}
\seealso{
\code{\link{get_job_qq_data}}
}
