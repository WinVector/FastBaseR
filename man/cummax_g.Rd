% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cummax.R
\name{cummax_g}
\alias{cummax_g}
\title{Compute per-group cumulative maxs}
\usage{
cummax_g(col, first_indices)
}
\arguments{
\item{col}{vector to cumulative max}

\item{first_indices}{numeric index of first index in each group}
}
\value{
per-group cumulative max
}
\description{
Compute per-group cumulative maxs
}
\examples{

v <- c(2, 3, 1, 7, 5)
fi <- c(1, 1, 3, 3, 3)
cummax_g(v, fi)

}
