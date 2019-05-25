#' Swap edges of a bipartite graph (method 2)
#'
#' @description Run the edge swapping algorithm from Milo et al. (2003) on a
#'   bipartite graph while maintaining the separation of the nodes in two groups.
#'
#' @param gr a tidygraph object with a node attribute called \code{type} that
#'   holds boolean values (i.e. either \code{TRUE} or \code{FALSE})
#' @param Q number of permutations you are conducting; default is 20
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
#' @export bipartite_edge_swap2
bipartite_edge_swap2 <- function(bgr, Q = 20, N = igraph::ecount(bgr)*Q) {
    # check for required node attribute "type"
    check_gr(bgr, "type")
    check_gr(bgr, "name")

    bgr_el <- to_bipartite_edgelist(bgr)
    n_edges <- length(bgr_el$nodes1)
    for (i in 1:N) {
        bgr_el <- swap_an_edge2(bgr_el, n_edges = n_edges, max_try = 100)
    }

    swapped_bgr <- edgelist_to_bipartite_graph(bgr_el)
    return(swapped_bgr)
}


#' Transform between a bipartite graph and edge list
#'
#' @description Turn the bipartite graph into an edglist returns a list of two
#'   vectors of the nodes for the edges, each corresponding to one of the two
#'   bipartite graph groups
#'
#' @param bgr bipartite graph with node attributes \code{type} and \code{name}
#' @param el edge list with two vectors for the edges, one for each set of nodes
#'   in the bipartite graph
#'
#' @importFrom magrittr %>%
#' @name bipartite_edgelist
#' @export to_bipartite_edgelist
to_bipartite_edgelist <- function(bgr) {
    if (igraph::ecount(bgr) < 1) stop("graph has no edges")
    if (igraph::vcount(bgr) < 1) stop("graph has no vertices")

    g1 <- tidygraph::as_tibble(bgr, active = "nodes") %>%
        dplyr::filter(type) %>% dplyr::pull(name) %>% unlist()
    g2 <- tidygraph::as_tibble(bgr, active = "nodes") %>%
        dplyr::filter(!type) %>% dplyr::pull(name) %>% unlist()
    el <- igraph::as_edgelist(bgr, names = TRUE)
    v1 <- unlist(apply(el, 1, function(edge) edge[edge %in% g1]))
    v2 <- unlist(apply(el, 1, function(edge) edge[edge %in% g2]))

    return(list(
        "nodes1" = v1,
        "nodes2" = v2
    ))
}


#' @rdname bipartite_edgelist
#' @export edgelist_to_bipartite_graph
edgelist_to_bipartite_graph <- function(el) {
    stop("Need to write this function")
    # TODO: make sure to include name and type node attributes
}


#' Swap a single edge (method 2)
#'
#' @description Swap an edge of a bipartite graph while maintaining the
#'   partition
#'
#' @param el edgelist composed of a list of two vectors, one for each group of
#'   the bipartite graph
#' @param max_try number of times to try to find two edges to swap. If no edges
#'   are found, the program will crash with the message "Unable to swap edges";
#'   default is 100
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
#' @export swap_an_edge2
swap_an_edge2 <- function(el, n_edges = length(el$nodes1), max_try = 100) {
    # check that `el` has the expected vectors
    if (!all(names(el) == c("nodes1", "nodes2"))) {
        stop(paste("'el' does not have the correctly named vectors:",
                   names(el)))
    }

    idx <- c()
    check <- TRUE
    try_counter <- 0
    while(check) {
        if (try_counter > max_try) stop("Unable to swap edges")
        try_counter <- try_counter + 1

        rand_e1 <- sample(c(1:n_edges), 1)
        rand_n11 <- el$nodes1[[rand_e1]]
        rand_n12 <- el$nodes2[[rand_e1]]

        adj_n12 <- unique(unlist(el$nodes1[el$nodes2 == rand_n12]))
        adj_n11 <- unique(unlist(el$nodes2[el$nodes1 == rand_n11]))

        idx <- ((el$nodes1 %in% adj_n12) | (el$nodes2 %in% adj_n11))

        # if TRUE: the random nodes are adjacent to all other nodes
        check <- all(idx)
    }
    rand_e2 <- sample(which(!idx), 1)
    el$nodes2[[rand_e1]] <- el$nodes2[[rand_e2]]
    el$nodes2[[rand_e2]] <- rand_n12

    return(el)
}
