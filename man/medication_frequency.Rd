% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{medication_frequency}
\alias{medication_frequency}
\title{ATC Code Frequency Within Clusters}
\usage{
medication_frequency(
  object,
  only = NULL,
  clusters = NULL,
  additional_data = NULL,
  ...
)
}
\arguments{
\item{object}{An object for which a summary is desired.}

\item{only}{<\code{\link[dplyr:dplyr_data_masking]{data-masking}}> Expressions that
return a logical value, and are defined in terms of the variables in
\code{object} and/or \code{additional_data}.

The default \code{NULL} selects all clusterings in \code{object}.}

\item{clusters}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}> An unquoted
expression naming the cluster or clusters in \code{object} one wants to
see summaries of. Names can be used as if they were positions in the data
frame, so expressions like I:IV can be used to select a range of clusters.

The default \code{NULL} selects all clusters in the chosen clusterings of
\code{object}.}

\item{additional_data}{A data frame with additional data that may be
(left-)joined onto the \code{parameters} in \code{object}. This is often
used in conjuction with \code{only} to select specific clusterings based on
\code{additional_data}.}

\item{...}{Additional arguments passed to the specific summary sub-function.}
}
\value{
\code{medication_frequency()} returns a data frame with class
\code{medication_frequency}.
\itemize{
\item \code{Clustering} the name of the clustering.
\item \code{Cluster} the cluster name.
\item \emph{atc} ATC codes.
\item \code{Count} number of individuals with this ATC code in this cluster.
\item \verb{Percent of All Medication} the percentage of individuals in the study
with this ATC code and cluster.
\item \verb{Percent of Medication in Cluster} the percent of individuals in the
cluster with this ATC code.
}
}
\description{
The function \code{medications()} calculates the frequency of the different
unique ATC codes within each cluster.
}
\details{
\code{medication_frequency()} calculates the number of individuals with a specific
ATC code within a cluster. Moreover, it calculates the percentage of people
with this medication assigned to this cluster and the percent of people
within the cluster with this medication.
}
\examples{
clust <- medic(complications, id = id, atc = atc, k = 3:5)

medication_frequency(clust, k == 5, clusters = I:III)

}
