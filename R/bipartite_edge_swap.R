#' Swap edges of a bipartite graph
#'
#' @description Run the edge swapping algorithm from Milo et al. (2003) on a
#'   bipartite graph while maintaining the separation of the nodes in two groups.
#'
#' @param gr a tidygraph object with a node attribute called \code{type} that
#'   holds boolean values (i.e. either \code{TRUE} or \code{FALSE})
#' @param Q number of permutations you are conducting
#' @param N the number of edge swaps; default is \eqn{Q \times |E(G)|}
#'
#' @return the graph with \eqn{N} random edge swaps with constrained marginals
#' @examples
#' set.seed(0)
#' bgr <- tidygraph::create_ring(6, directed = FALSE)
#' bgr <- tidygraph::mutate(bgr, type = rep(c(TRUE, FALSE), 3))
#' print(bgr)
#' print(bipartite_edge_swap(bgr, 10))
#'
#' @importFrom magrittr %>%
#' @importFrom tidygraph %E>% %N>%
#' @export bipartite_edge_swap
bipartite_edge_swap <- function(gr, Q, N = Q * igraph::ecount(gr)) {
    # check for required node attribute "type"
    check_gr(gr, "type")

    mod_gr <- gr %N>% tidygraph::mutate(.idx = 1:dplyr::n())

    nodes_1 <- get_node_index(gr, type)
    nodes_2 <- get_node_index(gr, !type)
    for (i in 1:N) {
        mod_gr <- swap_an_edge(mod_gr, nodes_1, nodes_2)
    }
    return(mod_gr)

}


#' Swap a single edge
#'
#' @description Swap an edge of a bipartite graph while maintaining the partition
#'
#' @param gr tidygraph graph object
#' @param n1,n2 vectors that indicate the grouping of the nodes (by index)
#' @param max_try Number of times to try to find two edges to swap. If no
#'   edges are found, the program will crash with the message "Unable to swap
#'   edges"
#'
#' @return a tidygraph graph object with two edges swapped
#'
#' @examples
#' set.seed(0)
#' library(tidygraph)
#' gr <- tidygraph::create_ring(6, directed = FALSE) %>%
#'     mutate(.idx = 1:n(), name = c("A", "b", "C", "d", "E", "f"))
#' swaped_gr <- swap_an_edge(gr, c(1,3,5), c(2,4,6))
#' plot(bind_graphs(gr, swaped_gr))
#'
#' @importFrom magrittr %>%
#' @export swap_an_edge
swap_an_edge <- function(gr, n1, n2, max_try = 100) {
    # attempt to get two edges to swap up to `max_tries` number of times
    e1 <- integer(0)
    e2 <- integer(0)
    try_counter <- 0
    while (length(e1) == 0 | length(e2) == 0) {
        if (try_counter > max_try) {
            stop("Unable to swap edges")
        }
        try_counter <- try_counter + 1
        # randomly select one edge
        e1 <- suppressMessages(random_edge_nodes(gr))
        # randomly select another edge where there no edges between the first set of nodes
        e2 <- suppressMessages(random_edge_nodes(gr, ignore_nodes_connected_to = e1))
    }

    # remove both sets of edges
    mod_gr <- remove_edge(gr, e1[[1]], e1[[2]]) %>%
        remove_edge(e2[[1]], e2[[2]])
    node_vec <- make_node_vector(e1, e2, n1, n2)
    new_edges <- tibble::tibble(
        from = node_vec[c(1, 3)],
          to = node_vec[c(4, 2)])
    tidygraph::bind_edges(mod_gr, new_edges)
}


# organize the nodes from the edges by their group in the bipartite graph
make_node_vector <- function(e1, e2, ns1, ns2) {
    A <- e1[e1 %in% ns1]  # edge 1, group 1
    B <- e1[e1 %in% ns2]  # edge 1, group 2
    C <- e2[e2 %in% ns1]  # edge 2, group 1
    D <- e2[e2 %in% ns2]  # edge 2, group 2
    return(c(A, B, C, D))
}




utils::globalVariables(c("type", ".idx"), add = TRUE)
