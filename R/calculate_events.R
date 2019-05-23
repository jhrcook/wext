#' Calculate the number of mutually exclusive or co-mutation events
#'
#' @description Calculate the number of mutually exclusive or co-mutation events
#'   in a bipartite graph
#'
#' @param gs the "gene set" is a vector of genes to consider
#' @param bgr a bipartite graph (as a tidygraph graph object); it must have a
#'   boolean node attribute "type" that describes which group each node is in
#' @param sample_is either \code{TRUE} or \code{FALSE} dictating which boolean
#'   value corresponds to the samples in the "type" attribute of \code{bgr}
#'
#' @examples
#' set.seed(0)
#' library(tidygraph)
#' bgr <- make_sample_gene_biprartite(
#'     simple_dataset$sample_name,
#'     simple_dataset$mutated_gene
#' )
#' plot(bgr)
#' calc_mutex_events(gs = c("a", "b"), bgr)
#' calc_comut_events(gs = c("c", "d"), bgr)
#'
#' @name calculate_events
#' @export calc_mutex_events
calc_mutex_events <- function(gs, bgr, sample_is = TRUE) {
    # check that the `type` node attribute exists
    check_gr(bgr, "type")

    gs <- unlist(gs)
    mutex_bgr <- bgr %N>%
        tidygraph::filter(type == !!sample_is | name %in% !!gs) %>%
        tidygraph::filter(type != !!sample_is |
                              tidygraph::centrality_degree(mode = "all") == 1)
    igraph::ecount(mutex_bgr)
}


#' @rdname calculate_events
#' @export calc_comut_events
calc_comut_events <- function(gs, bgr, sample_is = TRUE) {
    gs <- unlist(gs)
    comut_bgr <- bgr %N>%
        tidygraph::filter(type == !!sample_is | name %in% !!gs) %>%
        tidygraph::filter(type != !!sample_is |
                              tidygraph::centrality_degree(mode = "all") > 1) %>%
        tidygraph::filter(type == !!sample_is)
    igraph::vcount(comut_bgr)
}
