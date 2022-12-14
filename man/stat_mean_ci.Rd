% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_mean_ci.R
\name{stat_mean_ci}
\alias{stat_mean_ci}
\title{Mean Confidence Intervals}
\usage{
stat_mean_ci(x, alpha)
}
\arguments{
\item{x}{values}

\item{alpha}{alpha}
}
\value{
lower_ci, upper_ci
}
\description{
Mean Confidence Intervals
}
\examples{
stat_mean_ci(c(1, 2, 3, 4, 5, 6, 7), 1.96)
stat_mean_ci(c(2, 4, 6, 8), 1.96)
}
