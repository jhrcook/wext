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
#' @name random_edge_nodes
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

#' @rdname random_edge_nodes
#' @export random_edge_nodes2
random_edge_nodes2 <- function(gr, max_try = 100) {
    el <- to_bipartite_edgelist(gr)
    v1 <- unlist(el[1])
    v2 <- unlist(el[2])

    idx <- c()

    check <- TRUE
    try_counter <- 0
    while(check) {
        if (try_counter > max_try) stop("Unable to swap edges")
        try_counter <- try_counter + 1

        rand_e1 <- sample(c(1:length(v1)), 1)
        rand_n11 <- v1[[rand_e1]]
        rand_n12 <- v2[[rand_e1]]

        adj_n12 <- unique(unlist(v1[v2 == rand_n12]))
        adj_n11 <- unique(unlist(v2[v1 == rand_n11]))

        idx <- (v1 %in% c(rand_n11, adj_n12)) | (v2 %in% c(rand_n12, adj_n11))

        check <- length(idx) == sum(idx) # do not want these to equal each other
    }

    v1_cut <- v1[!idx]
    v2_cut <- v2[!idx]

    rand_e2 <- sample(c(1:length(v1_cut)), 1)
    rand_n21 <- v1_cut[[rand_e2]]
    rand_n22 <- v2_cut[[rand_e2]]

    return(list(
        "e1" = c(rand_n11, rand_n12),
        "e2" = c(rand_n21, rand_n22)
    ))
}
