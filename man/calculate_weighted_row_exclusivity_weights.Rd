% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_weighted_row_exclusivity_weights.R
\name{calculate_weighted_row_exclusivity_weights}
\alias{calculate_weighted_row_exclusivity_weights}
\title{Calculate the weights for the Weighted-Row-Exclusivity test}
\usage{
calculate_weighted_row_exclusivity_weights(dat, sample_col, mutgene_col)
}
\arguments{
\item{dat}{tibble with mutation information}

\item{sample_col}{column of samples names (quoted)}

\item{mutgene_col}{column of genes that are mutated (quoted)}
}
\description{
The weighted test for mutual exclusivity aims to calculate the
  probability of observing at least \eqn{t_M} mutually exclusive events given
  a fixed number of samples (row-sums) and a per-gene, per-sample weight
  matrix \eqn{W}. For further explanation, see the "Computing the Weighted
  Exclusivity Test (WExT)" vignette. This function creates the weight matrix
  for the test.
}
\examples{
library(wext)
calculate_weighted_row_exclusivity_weights(dat = simple_dataset,
                                           sample_col = sample_name,
                                           mutgene_col = mutated_gene)

}
