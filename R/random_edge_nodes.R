#' Return the nodes for a randomly selected edge
#'
#' @description Return the nodes of an edge in the graph \code{gr} selected at
#'   random. Edges from other nodes can be ignored by passing their index(es) to
#'   \code{ignore_nodes_connected_to}. This is used in swapping edges by passing
#'   the nodes for the first edge to be ignored in the selection of the second
#'   edge.
#'
#' @param gr tidygraph graph object; must have the node attribute \code{.idx}
#'   with the node indices
#' @param ignore_nodes_connected_to a vector of indices for the nodes whose
#'   edges to ignore
#'
#' @return a vector of two indices for the nodes of the edge selected; if no
#'   edge was able to be selected, then \code{integer(0)} is returned
#'
#' @examples
#' set.seed(0)
#' gr <- tidygraph::create_ring(5)
#' gr <- tidygraph::mutate(gr, .idx = 1:dplyr::n())
#' random_edge_nodes(gr)
#'
#' @importFrom magrittr %>%
#' @importFrom tidygraph %N>% %E>%
#' @export random_edge_nodes
random_edge_nodes <- function(gr, ignore_nodes_connected_to = c()) {
    # check that the node attribute ".idx" exists
    check_gr(gr, ".idx")

    # check an edge exists
    if (igraph::ecount(gr) < 1) {
        stop("graph does not contain any edges to pull from")
    }

    ig_nodes <- unlist(ignore_nodes_connected_to)  # easier to use in the pipeline
    gr %N>%
        tidygraph::filter(!tidygraph::node_is_adjacent(to = ig_nodes,
                                                       mode = "all",
                                                       include_to = TRUE)) %E>%
        tidygraph::sample_n(1) %N>%
        tidygraph::filter(tidygraph::centrality_degree(mode = "all") > 0) %>%
        tidygraph::as_tibble(active = "nodes") %>%
        dplyr::pull(.idx) %>%
        unlist()
}

utils::globalVariables(c(".idx"), add = TRUE)
