#' Calculate the weights for the Row-Column-Exclusivity test
#'
#' @description The weight for each mutation in the Row-Column-Exclusivity test
#'   cannot be calculated exactly for most use cases because there is no closed
#'   formula and a very large number of possible matrices to consider. Thus, the
#'   weights are calculated emperically from a sufficiently large number of
#'   samples of possible matrices.
#'
#' @param dat tibble with mutation information
#' @param sample_col column of samples names (quoted)
#' @param mutgene_col column of genes that are mutated (quoted)
#' @param Q number of permutation matrices to use (default is 100)
#'
#' @examples
#' library(wext)
#' calculate_row_col_exclusivity_weights(dat = simple_dataset,
#'                                       sample_col = sample_name,
#'                                       mutgene_col = mutated_gene,
#'                                       N = 1E4)
#'
#' @importFrom magrittr %>%
#' @export calculate_row_col_exclusivity_weights
calculate_row_col_exclusivity_weights <- function(dat, sample_col, mutgene_col,
                                                  Q = 100) {
    sample_col <- rlang::enquo(sample_col)
    mutgene_col <- rlang::enquo(mutgene_col)

    n <- rlang::eval_tidy(sample_col, dat) %>%
        unlist() %>%
        dplyr::n_distinct()
    if (n < 2) stop("Not enough unique samples to compare.")

    bipartite_gr <- make_sample_gene_biprartite(
        rlang::eval_tidy(sample_col, dat),
        rlang::eval_tidy(mutgene_col, dat)
    )

    # run Q edge swap permutations
    tib <- dat %>%
        dplyr::select(!!sample_col, !!mutgene_col) %>%
        unique() %>%
        dplyr::mutate(edge_sum = 0)
    names(tib) <- c("samples", "genes", "edge_sum")
    for (i in 1:Q) {
        perm_gr <- edge_swap(bipartite_gr, Q = Q) %>%
            igraph::as_edgelist(names = TRUE)
        colnames(perm_gr) <- names(tib)[1:2]
        perm_gr <-tibble::as_tibble(perm_gr) %>%
            mutate(edge_sum = 1)
        print(perm_gr)
        stop()
        tib <- dplyr::bind_rows(tib, perm_gr) %>%
            dplyr::group_by(samples, genes) %>%
            dplyr::summarise(edge_sum = sum(edge_sum)) %>%
            ungroup()
    }
    print(tib)
    tib <- tib %>%
        dplyr::mutate(row_col_ex_weights = edge_sum / !!Q) %>%
        dplyr::select(!!sample_col, !!mutgene_col, row_col_ex_weights)
    return(tib)
}


# make the sample-gene bipartite graph
make_sample_gene_biprartite <- function(s, g) {
    bgr <- tibble::tibble(samples = s, genes = g) %>%
        tidygraph::as_tbl_graph(directed = FALSE) %>%
        igraph::as.igraph()
    return(bgr)
}


# edge swap algorithm from Milo et al., 2003
# gr: an igraph graph object
# N: number of edges to swap
edge_swap <- function(gr, Q, N = NULL) {
    if (is.null(N)) N <- Q * igraph::ecount(gr)
    igraph::rewire(gr, igraph::keeping_degseq(loops = FALSE, niter = N))
}


# PROBLEMS:
#  1. edge_swap is not maintaining bipartition
#  2. column names of the final tibble do not match those from the original data
