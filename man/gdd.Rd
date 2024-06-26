% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdd.R
\name{gdd}
\alias{gdd}
\title{Calculate Growing Degree Days (GDD)}
\usage{
gdd(x, end_date = as.Date("1972-09-30"), min_length = 60, msgs = TRUE)
}
\arguments{
\item{x}{A data frame with two columns \code{date} and \code{temperature}.
\code{date}, which must be of class Date provides the dates and
\code{temperature} which must be a numeric vector provides the
mean daily water temperature in degrees centigrade.}

\item{end_date}{A Date scalar of the last date
within each year to consider (the year is ignored).}

\item{min_length}{A whole number of the minimum length to consider.
Must be at least twice the window width but no more than 365.
If NULL then set to be the difference between \code{start_date} and \code{end_date}
(ignoring the year).}

\item{msgs}{A flag specifying whether to provide messages.}
}
\description{
A wrapper on \code{\link[gsdd:gdd]{gsdd::gdd()}} to get the Growing Degree Days up to a date for
the longest growing season.
}
\examples{
gdd(gsdd::temperature_data)
}
\seealso{
\code{\link[gsdd:gdd]{gsdd::gdd()}}, \code{\link[=gsdd]{gsdd()}} and \code{\link[=gss]{gss()}}.
}
