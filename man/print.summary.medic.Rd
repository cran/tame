% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary-utils.R
\name{print.summary.medic}
\alias{print.summary.medic}
\title{Print Summary of Medication}
\usage{
\method{print}{summary.medic}(x, ...)
}
\arguments{
\item{x}{An object of class \code{summary.medic}.}

\item{...}{currently only included for compatibility with generic. Has no
effect.}
}
\value{
The function is called for its side effects and does not return any
value.
}
\description{
This function prints a summary of medication information.
}
\details{
This function prints various information about medication, including
cluster frequency, medication frequency, number of different medication taken
in the study period, average exposure trajectories, and average exposure
trajectories by ATC groups.
}
