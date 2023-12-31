% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsdd-cf.R
\name{gsdd_cf}
\alias{gsdd_cf}
\title{Calculate Growing Season Degree Days (GSDD)}
\usage{
gsdd_cf(
  x,
  ignore_truncation = FALSE,
  start_temp = 5,
  end_temp = 4,
  window_width = 7,
  quiet = FALSE
)
}
\arguments{
\item{x}{A numeric vector of mean daily water temperature data from
before to after the growing season in C. It must be at least 55
and no more than 366 days in length.}

\item{ignore_truncation}{A flag specifying whether to ignore truncation
when calculating the GSDD or a string of "left", "right", "none" or "both"
specifying which type of truncation to ignore.}

\item{start_temp}{A number of the average water temperature
at the start of the growing season in C.}

\item{end_temp}{A number of the average water temperature
at the end of the growing season in C.}

\item{window_width}{A positive whole number of the
width of the rolling mean window in days.}

\item{quiet}{A flag specifying whether to suppress warnings.}
}
\value{
A number of the GSDD.
}
\description{
Growing Season Degree Days (GSDD) is a water temperature metric
that is a useful predictor of Cutthroat trout size at the
beginning of winter. It is the accumulated thermal units (in C)
during the growing season based on the mean daily water temperature values.
}
\details{
By default the growing season is as defined by
Coleman and Fausch (2007) who stated that

We defined the start of the growing season as the
beginning of the first week that average stream temperatures exceeded and
remained above 5C for the season;
the end of the growing season was defined as
the last day of the first week that
average stream temperature dropped below 4C.

For the purposes of the calculation week is assumed to refer to a seven day
rolling average as opposed to the calendar week and if there are multiple
start and/or end dates the growing season is assumed to be the period of time
with the highest GSDD between the start and end dates.
}
\examples{
x <- c(rep(1, 10), rep(10, 20), rep(1, 200))
gsdd_cf(x)

x <- teckfish::simulated_data$synthetic
output <- gsdd_cf(x)
print(output)
}
