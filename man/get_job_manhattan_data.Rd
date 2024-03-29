% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manhattan.R
\name{get_job_manhattan_data}
\alias{get_job_manhattan_data}
\title{Get Manhattan plot data}
\usage{
get_job_manhattan_data(job, token = get_current_access_token())
}
\arguments{
\item{job}{A job ID string (UUID)}

\item{token}{An Encore API access token}
}
\value{
This will return an object of type "encore_manhattan_data"
that can be used with the `plot_manhattan` function.
}
\description{
This function fetches Manhattan plot data from API Server
}
\details{
This function will return the data used to draw
the Manhattan plot on the Encore website. Note
that the data has been summarized on the server
and binned in order to reduce the number of points
to be drawn.
}
\examples{
\dontrun{
job_id <- "123e4567-e89b-12d3-a456-426652340000"
data <- get_job_manhattan_data(job_id)
plot_manhattan(data)
}
}
\seealso{
\code{\link{plot_manhattan}}
}
