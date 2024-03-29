% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/territory.R
\name{territory_by_size}
\alias{territory_by_size}
\title{Territory Size Requirement}
\source{
IP-117068
}
\usage{
territory_by_size(
  fork_lengths = c(mean(c(3.5, 4.2)), mean(c(4.2, 7.2)), rep(mean(c(7.2, 11)), 2))
)
}
\arguments{
\item{fork_lengths}{The fork length in centimeters. The default value is the
mid-point for each size class.}
}
\description{
Territory requirement as a function of fork length \href{https://cdnsciencepub.com/doi/10.1139/f90-197}{Grant and Kramer 1990.}
Default value returns the territory requirement in square meters for each size class.
}
