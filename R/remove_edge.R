#' Remove an edge
#'
#' @description Remove the edge between \code{a} and \code{b}
#'
#' @param gr tidygraph graph object
#' @param a,b indices of the two nodes connected by the edge to remove
#'
#' @return a tidygraph graph object without the edge
#'
#' @examples
#' set.seed(0)
#' plot(remove_edge(tidygraph::create_ring(3), 1, 2))
#'
#' @importFrom tidygraph %E>%
#' @export remove_edge
remove_edge <- function(gr, a, b) {
    gr %E>%
        tidygraph::filter(!tidygraph::edge_is_between(a, b, ignore_dir = TRUE))
}
