#' The Row-Column-Exclusivity test
#'
#' @description The Row-Column-Exclusivity test cannot be calculated exactly for
#'   most use cases because there is no closed formula and a very large number
#'   of possible matrices to consider. Thus, the probabilities are calculated
#'   empirically from a sufficiently large number of samples of possible
#'   matrices.
#'
#' @param dat tibble with mutation information
#' @param sample_col column of samples names (quoted)
#' @param mutgene_col column of genes that are mutated (quoted)
#' @param k size of gene sets to consider (default is 2)
#' @param seed_genes a vector of gene(s) that must be in the gene set to be tested (optional)
#' @param N_perms number of permutation matrices to use (default is 10,000)
#' @param min_t_AM minumum number of real mutual exclusive events required to
#'   consider the gene set (default is 2)
#'
#' @examples
#' library(wext)
#' calculate_row_col_exclusivity_weights(dat = simple_dataset,
#'                                       sample_col = sample_name,
#'                                       mutgene_col = mutated_gene,
#'                                       k = 2,
#'                                       Q = 5)
#'
#' @importFrom magrittr %>%
#' @importFrom tidygraph %N>%
#' @importFrom rlang !!
#' @export rc_test
rc_test <- function(dat, sample_col, mutgene_col,
                    k = 2,
                    seed_genes = c(),
                    N_perms = 1e4,
                    min_t_AM = 2) {
    # get original column names to use later
    original_colnames <- c(
        rlang::as_string(rlang::ensym(sample_col)),
        rlang::as_string(rlang::ensym(mutgene_col))
    )

    # enquote the input column names
    sample_col <- rlang::enquo(sample_col)
    mutgene_col <- rlang::enquo(mutgene_col)

    # number of unique samples
    n <- rlang::eval_tidy(sample_col, dat) %>% unlist() %>% dplyr::n_distinct()
    if (n < 2) stop("Not enough unique samples to compare.")

    # vector of all genes
    genes <- rlang::eval_tidy(mutgene_col, dat) %>% unlist() %>% unique()

    # make bipartite graph for the edge swapping
    bipartite_gr <- make_sample_gene_biprartite(
        rlang::eval_tidy(sample_col, dat),
        rlang::eval_tidy(mutgene_col, dat)
    )

    results_tib <- make_empty_results_tracker(genes, k, seed_genes = seed_genes) %>%
        dplyr::mutate(
            t_AM = purrr::map(gene_sets, calc_mutex_events, bgr = bipartite_gr)
        ) %>%
        dplyr::filter(min_t_AM > 2)

    for (i in 1:N_perms) {
        perm_bgr <- bipartite_edge_swap(bipartite_gr, Q = 100)
        results_tib <- purrr::pmap_df(
            results_tib, update_results_tib, bgr = perm_bgr
        )
    }
    results_tib <- results_tib %>%
        dplyr::mutate(p_val = t_BM_ge / N_perms)
    return(results_tib)
}



# make the sample-gene bipartite graph
make_sample_gene_biprartite <- function(s, g) {
    bgr <- tibble::tibble(samples = s, genes = g) %>%
        tidygraph::as_tbl_graph(directed = FALSE) %N>%
        tidygraph::mutate(type = name %in% s)
    return(bgr)
}



# start the results tracking tibble with the gene sets and t_BM_gr = 0
make_empty_results_tracker <- function(gs, k, seed_genes) {
    # TODO: include seed genes
    combs <- combn(unlist(gs), k) %>%
        apply(1, list)
    if (length(seed_genes) > 0) {
        idx <- purrr::map_lgl(gs, ~ any(seed_genes %in% .x))
        combs <- combs[idx]
    }
    tibble::tibble(gene_sets = combs, t_BM_ge = 0)
}



# calculate the number of mutually exclusive events
calc_mutex_events <- function(gs, bgr, sample_is = TRUE) {
    gr <- unlist(gs)
    mutex_bgr <- bgr %N>%
        tidygraph::filter(type == !!sample_is | name %in% !!gs) %>%
        tidygraph::filter(type != !!sample_is |
                          tidygraph::centrality_degree(mode = "all") == 1)
    igraph::ecount(mutex_bgr)
}



# update the gene sets in the results tracking tibble using the edge-swapped bgr
update_results_tib <- function(gene_sets, t_BM_ge, t_AM, bgr) {
    t_BM <- calc_mutex_events(gene_sets, bgr)
    if (t_BM >= t_AM) {
        t_BM_ge <- t_BM_ge + 1
    }
    tibble::tibble(gene_sets = list(gene_sets),
                   t_BM_ge = t_BM_ge,
                   t_AM = t_AM)
}
