% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bipartite_edge_swap2.R
\name{bipartite_edgelist}
\alias{bipartite_edgelist}
\alias{to_bipartite_edgelist}
\alias{edgelist_to_bipartite_graph}
\title{Transform between a bipartite graph and edge list}
\usage{
to_bipartite_edgelist(bgr)

edgelist_to_bipartite_graph(el, sample_list = 1)
}
\arguments{
\item{bgr}{bipartite graph with node attributes \code{type} and \code{name}}

\item{el}{edge list with two vectors for the edges, one for each set of nodes
in the bipartite graph}

\item{sample_list}{which vector in `el` corresponds to the samples}
}
\description{
Turn the bipartite graph into an edge list returns a list of two
  vectors of the nodes for the edges, each corresponding to one of the two
  bipartite graph groups
}
