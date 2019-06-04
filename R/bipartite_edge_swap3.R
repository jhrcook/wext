#' Swap edges of a bipartite graph (method 3)
#'
#' @description Run the edge swapping algorithm from Milo et al. (2003) on a
#'   bipartite graph while maintaining the separation of the nodes in two
#'   groups.
#'
#' @param gr a tidygraph object with a node attribute called \code{type} that
#'   holds boolean values (i.e. either \code{TRUE} or \code{FALSE})
#' @param Q number of permutations you are conducting; default is 20
#' @param N the number of edge swaps; default is \eqn{Q \times |E(G)|}
#' @param quiet boolean for if you want the number of successful edge swaps
#'   printed; default \code{TRUE}
#'
#' @return the graph with \eqn{N} random edge swaps with constrained marginals
#' @examples
#' set.seed(0)
#' bgr <- tidygraph::create_ring(6, directed = FALSE)
#' bgr <- tidygraph::mutate(
#'     bgr,
#'     type = rep(c(TRUE, FALSE), 3),
#'     name = LETTERS[1:6])
#' print(bgr)
#' print(bipartite_edge_swap3(bgr, 10))
#'
#' @importFrom magrittr %>%
#' @importFrom tidygraph %E>% %N>%
#' @export bipartite_edge_swap3
bipartite_edge_swap3 <- function(gr, Q = 20, N = igraph::ecount(gr)*Q, quiet = TRUE) {
    # check for required node attribute "type"
    check_gr(gr, "type")
    check_gr(gr, "name")

    gr_el <- to_bipartite_edgelist(gr)
    n1 <- unlist(gr_el$nodes1)
    n2 <-unlist(gr_el$nodes2)

    n1_idx <- unique(n1)
    n2_idx <- unique(n2)

    n1_num <- match(n1, n1_idx)
    n2_num <- match(n2, n2_idx)

    # use an function implemented in C++ using Rcpp
    n1_num <- swap_an_edgeC(n1_num, n2_num, N = N, max_try = 100, quiet = quiet)

    swapped_el <- list(
        "nodes1" = n1_idx[n1_num],
        "nodes2" = n2
    )

    swapped_gr <- edgelist_to_bipartite_graph(swapped_el)
    return(swapped_gr)
}
