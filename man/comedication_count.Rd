% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{comedication_count}
\alias{comedication_count}
\title{Frequency tables for medication amount}
\usage{
comedication_count(
  object,
  only = NULL,
  clusters = NULL,
  count_grouper = function(x) {
     cut(x, breaks = c(0, 1, 2, Inf), labels = c("1",
    "2", "3+"))
 },
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

\item{count_grouper}{A function for grouping counts. As a standard it groups
counts as 1 medication, 2 medications, and 3+ medications.}

\item{additional_data}{A data frame with additional data that may be
(left-)joined onto the \code{parameters} in \code{object}. This is often
used in conjuction with \code{only} to select specific clusterings based on
\code{additional_data}.}

\item{...}{Additional arguments passed to the specific summary sub-function.}
}
\value{
\code{comedication_count()} returns a data frame of class
\code{comedication_count}
\itemize{
\item \code{Clustering} the name of the clustering.
\item \code{Cluster} the name of the cluster.
\item \verb{Medication Count} a number of medications. The numbers or groups are
given by the \code{count_grouper()} function.
\item \verb{Number of People} the number of individuals in cluster who has
\verb{Medication Count} number of comedications in study.
\item \verb{Number of medications} the number of medications of individuals who has
\verb{Medication Count} number of comedications in the cluster.
\item \verb{Percentage of All People} the percentage of individuals is study who has
\verb{Medication Count} number of comedications in the cluster.
\item \verb{Percentage of People in Cluster} the percentage of individuals in the
cluster who has \verb{Medication Count} number of comedications.
\item \verb{Percentage of All Medications} the percentage of medication in study from
individuals who has \verb{Medication Count} number of comedications in cluster.
\item \verb{Percentage of Medication in Cluster} the percentage of medication in
cluster from individuals who has \verb{Medication Count} number of
comedications.
\item \verb{Percentage of People with the Same Medication Count} percentage of
individuals among those with \verb{Medication Count} number of comedications in
this cluster.
\item \verb{Percentage of Medication with the Same Medication Count} percentage of
medication among medication of individuals with \verb{Medication Count} number
of comedications in this cluster.
}
}
\description{
The function \code{comedication_count()} calculates the number of unique
medications for each individual and presents the count frequencies by
cluster.
}
\details{
\code{comedication_count()} calculates the number of ATC codes an individual has,
and then outputs the number of individuals within a cluster that has that
many ATC codes. Moreover, various relevant percentages or calculated. See
Value below for more details on these percentages.
}
\examples{
clust <- medic(complications, id = id, atc = atc, k = 3:5)

comedication_count(clust, k == 5, clusters = I:III)

}
