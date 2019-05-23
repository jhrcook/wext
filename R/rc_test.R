#' The Row-Column-Exclusivity or Co-mutation test
#'
#' @description The Row-Column-Exclusivity or Co-mutation test cannot be
#'   calculated exactly for most use cases because there is no closed formula
#'   and a very large number of possible matrices to consider. Thus, the
#'   probabilities are calculated empirically from a sufficiently large number
#'   of samples of possible matrices.
#'
#' @param dat tibble with mutation information
#' @param sample_col column of samples names (quoted)
#' @param mutgene_col column of genes that are mutated (quoted)
#' @param k size of gene sets to consider (default is 2)
#' @param which_test test for mutual exclusivity (\code{"exclusivity"}) or
#'   co-mutation (\code{"comutation"}); (default is \code{"comutation"})
#' @param seed_genes a vector of gene(s) that must be in the gene set to be
#'   tested (optional)
#' @param N_perms number of permutation matrices to use (default is 10,000)
#' @param min_mutex_events minimum number of real mutual exclusive events
#'   required to consider the gene set (default is 2)
#'
#' @examples
#' library(wext)
#' rc_test(simple_dataset, sample_name, mutated_gene, k = 2, N_perms = 5)
#'
#' @importFrom magrittr %>%
#' @importFrom tidygraph %N>%
#' @importFrom rlang !!
#' @export rc_test
rc_test <- function(dat, sample_col, mutgene_col,
                    k = 2,
                    which_test = c("exclusivity", "comutation"),
                    seed_genes = c(),
                    N_perms = 1e4,
                    min_mutex_events = 2) {

    # choose only one test
    which_test <- stringr::str_to_lower(which_test[[1]])
    if (which_test %in% c("exclusivity", "e")) {
        test_func <- calc_mutex_events
    } else if (which_test %in% c("comutation", "c")) {
        test_func <- calc_comut_events
    } else {
        stop(paste(which_test, "is not an option."))
    }

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
            t_AM = purrr::map_dbl(gene_sets, test_func, bgr = bipartite_gr)
        ) %>%
        dplyr::filter(t_AM > !!min_mutex_events)
    if (nrow(results_tib) < 1) {
        stop("No gene sets to test that pass the minumum number of real mutaully exclusvie events")
    }

    for (i in 1:N_perms) {
        perm_bgr <- bipartite_edge_swap(bipartite_gr, Q = 100)
        results_tib <- purrr::pmap_df(
            results_tib, update_results_tib, f = test_func, bgr = perm_bgr
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
    combs <- utils::combn(unlist(gs), k) %>%
        apply(2, list) %>%
        unlist(recursive = FALSE)
    if (length(seed_genes) > 0) {
        idx <- purrr::map_lgl(gs, ~ any(seed_genes %in% .x))
        combs <- combs[idx]
    }
    tibble::tibble(gene_sets = combs, t_BM_ge = 0)
}



# update the gene sets in the results tracking tibble using the edge-swapped bgr
update_results_tib <- function(gene_sets, t_BM_ge, t_AM, f, bgr) {
    t_BM <- f(gene_sets, bgr)
    if (t_BM >= t_AM) {
        t_BM_ge <- t_BM_ge + 1
    }
    tibble::tibble(gene_sets = list(gene_sets),
                   t_BM_ge = t_BM_ge,
                   t_AM = t_AM)
}


utils::globalVariables(
    c("type","name", "t_BM_ge", "gene_sets", "t_AM"),
    add = TRUE
)
