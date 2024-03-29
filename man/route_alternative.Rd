% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/route.R
\name{route_alternative}
\alias{route_alternative}
\title{Route Natal Streams - Alternative}
\source{
IP-117068
}
\usage{
route_alternative(
  year,
  month,
  juveniles,
  inchannel_habitat,
  floodplain_habitat,
  prop_pulse_flows,
  .pulse_movement_intercept,
  .pulse_movement_proportion_pulse,
  .pulse_movement_medium,
  .pulse_movement_large,
  .pulse_movement_vlarge,
  .pulse_movement_medium_pulse,
  .pulse_movement_large_pulse,
  .pulse_movement_very_large_pulse,
  territory_size,
  temperature_downstream = 19,
  density_dependent_survival,
  stochastic
)
}
\arguments{
\item{year}{The current simulation year, 1-20}

\item{month}{The current simulation month, 4-11}

\item{inchannel_habitat}{A vector of available habitat in square meters}

\item{floodplain_habitat}{A vector of available floodplain habitat in square meters}

\item{prop_pulse_flows}{The proportion of pulse flows}

\item{.pulse_movement_intercept}{Intercept for \code{\link{pulse_movement}}}

\item{.pulse_movement_proportion_pulse}{Coefficient for \code{\link{pulse_movement}} \code{proportion_pulse} variable}

\item{.pulse_movement_medium}{Size related intercept for \code{\link{pulse_movement}} medium sized fish}

\item{.pulse_movement_large}{Size related intercept for \code{\link{pulse_movement}} large sized fish}

\item{.pulse_movement_vlarge}{Size related intercept for \code{\link{pulse_movement}} very large sized fish}

\item{.pulse_movement_medium_pulse}{Additional coefficient for \code{\link{pulse_movement}} \code{proportion_pulse} variable for medium size fish}

\item{.pulse_movement_large_pulse}{Additional coefficient for \code{\link{pulse_movement}} \code{proportion_pulse} variable for large size fish}

\item{.pulse_movement_very_large_pulse}{Additional coefficient for \code{\link{pulse_movement}} \code{proportion_pulse} variable for very large size fish}

\item{territory_size}{Array of juvenile fish territory requirements for \code{\link{fill_natal}}}

\item{temperature_downstream}{TODO}

\item{juvenile}{An n by 4 matrix of juvenile fish by watershed and size class}

\item{detour}{Values can be 'sutter' or 'yolo' if some juveniles are detoured on to that bypass, otherwise NULL}
}
\description{
Determines if juveniles stay in their natal tributary, are detoured
to a bypass, or out migrate during a simulated month
}
\details{
See \code{\link{params}} for details on parameter sources
}
