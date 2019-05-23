#' Make the sample-gene bipartite graph
#'
#' @description Create a bipartite graph connecting cancer samples and their
#'   mutations.
#'
#' @param s vector of samples
#' @param g vector of mutated genes
#'
#' @return a tidygraph graph object with node attributes \code{name} and
#'   \code{type}. Attribute \code{type} holds boolean values to indicate which
#'   of the two groups the node belongs to - \code{TRUE} is for samples.
#'
#' @examples
#' make_sample_gene_bipartite(
#'     simple_dataset$sample_name,
#'     simple_dataset$mutated_gene
#' )
#'
#' @importFrom tidygraph %N>%
#' @importFrom magrittr %>%
#' @export make_sample_gene_bipartite
make_sample_gene_bipartite <- function(s, g) {
    bgr <- tibble::tibble(samples = s, genes = g) %>%
        tidygraph::as_tbl_graph(directed = FALSE) %N>%
        tidygraph::mutate(type = name %in% s)
    return(bgr)
}

utils::globalVariables(c("name"), add = TRUE)
