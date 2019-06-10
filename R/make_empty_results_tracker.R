#' Initialize the mutation event tracker tibble
#'
#' @description A tibble that contains the gene sets to consider mutual
#'   exclusivity or co-mutation between. This uses a bit of logic to reduce the
#'   number of gene sets to consider by accounting the for test at hand
#'   (exclusivity or co-mutation), the minimum mutations in a gene to consider
#'   it in any gene sets, and a seed gene is the user is interested in
#'   interactions with only one or a few genes.
#'
#' @param bgr a bipartite edge graph that has the node attributes \code{type}
#'   and \code{name}; these attributes are not checked for explicitly, but will
#'   throw an error if not present
#' @param k size of gene sets to consider (default is 2)
#' @param which_test test for mutual exclusivity (\code{"exclusivity"}) or
#'   co-mutation (\code{"comutation"}); (default is \code{"comutation"})
#' @param seed_genes a vector of gene(s) that must be in the gene set to be
#'   tested (optional)
#' @param min_times_mut minimum number of times a gene must be mutated in all
#'   samples to be considered for the gene sets (default is 5)
#'
#' @return A tibble with two columns: \code{gene_sets} contains list objects of
#'   \code{k} genes, \code{t_BM_gr} is a column of zeros that will eventually
#'   hold the number of events (either mutual exclusivity or co-mutation) that
#'   occur between the genes in the gene sets in the permuted bipartite graphs
#'
#' @examples
#' library(wext)
#' bgr <- make_sample_gene_bipartite(
#'     simple_dataset$sample_name,
#'     simple_dataset$mutated_gene
#' )
#' make_empty_results_tracker(bgr, 2, "exclusivity", c(), 2)
#'
#' @importFrom tidygraph %N>%
#' @importFrom magrittr %>%
#' @name results_tracker
#' @export make_empty_results_tracker
make_empty_results_tracker <- function(bgr, k, which_test,
                                       seed_genes, min_times_mut) {
    if (which_test %in% c("exclusivity", "e")) {
        return(exclusivity_results_tracker(bgr, k, seed_genes, min_times_mut))
    } else if (which_test %in% c("comutation", "c")) {
        return(comutation_results_tracker(bgr, k, seed_genes, min_times_mut))
    } else {
        stop(paste0("'", which_test, "' is not a choice of test."))
    }
}


#' @describeIn results_tracker Create the tracker when testing for mutual
#'   exclusivity.
#' @export exclusivity_results_tracker
exclusivity_results_tracker <- function(bgr, k, seed_genes, min_times_mut) {
    genes <- filter_by_mutation_frequency(bgr, min_times_mut)
    make_combs_tibble(genes, k, seed_genes)
}

#' @describeIn results_tracker Create the tracker when testing for co-mutation.
#' @export comutation_results_tracker
comutation_results_tracker <- function(bgr, k, seed_genes, min_times_mut) {
    mut_genes <- filter_by_mutation_frequency(bgr, min_times_mut)
    seeds_idx <- wext::get_node_index(bgr, name %in% seed_genes)
    genes <- bgr %N>%
        tidygraph::filter(
            !type | tidygraph::node_is_adjacent(seeds_idx,
                                                mode = "all",
                                                include_to = TRUE)
        ) %>%
        tidygraph::filter(name %in% mut_genes) %N>%
        tidygraph::as_tibble() %>%
        dplyr::pull(name) %>%
        unlist() %>%
        unique()
    make_combs_tibble(genes, k, seed_genes)
}

#' @describeIn results_tracker Create the final tibble object with the gene sets
#'   of size \code{k}.
#' @param genes a vector of gene(s) to make combinations of size \code{k} from
#' @export make_combs_tibble
make_combs_tibble <- function(genes, k, seed_genes) {
    combs <- utils::combn(unlist(genes), k) %>%
        apply(2, list) %>%
        unlist(recursive = FALSE)
    if (length(seed_genes) > 0) {
        idx <- purrr::map_lgl(combs, ~ any(seed_genes %in% .x))
        combs <- combs[idx]
    }
    tibble::tibble(gene_sets = combs, t_BM_ge = 0)
}


utils::globalVariables(
    c("type","name", "t_BM_ge", "gene_sets", "t_AM"),
    add = TRUE
)
