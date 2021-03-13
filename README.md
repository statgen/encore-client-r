
# Encore-client-R

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This is an R package that allows you to programmatically interact with the [Encore analysis server](https://encore.sph.umich.edu/).

Note that this is a preliminary release. Functions
may change overtime and you should not write too much
code that depends on these behaviors just yet.

Currently most functions focus on interacting with existing jobs
rather than creating new ones.

## Installation

You can install the development version of this package with:

``` r
remotes::install_github("statgen/encore-client-r")
```

## Example

In order to interact with Encore, you will need to 
supply an Encore API access token. You can create
one of those at https://encore.sph.umich.edu/me/api-token

``` r
library(encore)

# First, set your access token
# If you are using RStudio, run this and enter your token in the popup

set_access_token()

# Another option is to set an ENCORE_API_TOKEN environment variable.
# This can be done at the command line before launching R or you
# can set this value in a .Renviron file.
#
# Finally you can also choose to pass your token as a string 
# (but make sure not to share your token with others if you share
# your scripts).
#
# Note your actual token will be much longer; this it not a valid token

set_access_token("eyJhbGciOi9.eyJuYW1lIjoiRW5.e30XBiuDHBYDxSLT")

# After you set your token, you can interact with your job data

my_jobs <- get_jobs()
last_job <- head(my_jobs, 1)

plot_manhattan(last_job)
download_job_output(last_job)

```
