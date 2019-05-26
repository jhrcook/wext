#' Filter a bipartite graph's genes by number of mutations
#'
#' @description Returns a vector of genes that passes the minimum number of
#'   mutations.
#'
#' @param bgr a bipartite edge graph that has the node attributes \code{type}
#'   and \code{name}; these attributes are not checked for explicitly, but will
#'   throw an error if not present
#' @param min_times_mut minimum number of mutations per gene
#' @param sample_is either \code{TRUE} or \code{FALSE} dictating which boolean
#'   value corresponds to the samples in the "type" attribute of \code{bgr}
#'
#' @return a vector of (unique) gene names
#'
#' @examples
#' library(wext)
#' suppressPackageStartupMessages(library(tidygraph))
#' bgr <- make_sample_gene_bipartite(
#'     simple_dataset$sample_name,
#'     simple_dataset$mutated_gene
#' )
#' filter_by_mutation_frequency(bgr, 2)
#' filter_by_mutation_frequency(bgr, 3)
#' filter_by_mutation_frequency(bgr, 4, sample_is = FALSE)
#'
#' @importFrom rlang !!
#' @export filter_by_mutation_frequency
filter_by_mutation_frequency <- function(bgr, min_times_mut, sample_is = TRUE) {
    bgr %N>%
        tidygraph::mutate(deg = tidygraph::centrality_degree(mode = "all")) %>%
        tidygraph::filter(type != !!sample_is) %>%
        tidygraph::as_tibble() %>%
        dplyr::filter(deg >= !!min_times_mut) %>%
        dplyr::pull(name) %>%
        unlist() %>%
        unique()
}


utils::globalVariables(
    c("type","name", "deg"),
    add = TRUE
)
