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
#'                                       Q = 5)
#'
#' @importFrom magrittr %>%
#' @importFrom tidygraph %N>%
#' @export calculate_row_col_exclusivity_weights
calculate_row_col_exclusivity_weights <- function(dat, sample_col, mutgene_col,
                                                  Q = 100) {

    original_colnames <- c(
        rlang::as_string(rlang::ensym(sample_col)),
        rlang::as_string(rlang::ensym(mutgene_col))
    )

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
    cat("edge-swap number: ")
    for (i in 1:Q) {
        cat(i, " ")
        perm_el <- bipartite_edge_swap(bipartite_gr, Q = Q) %>%
            to_edgelist(col_names = original_colnames)
        tib <- dplyr::bind_rows(tib, perm_el) %>%
            dplyr::group_by(!!sample_col, !!mutgene_col) %>%
            dplyr::summarise(edge_sum = sum(edge_sum)) %>%
            dplyr::ungroup()
    }
    cat("\n")
    tib <- tib %>%
        dplyr::filter(!is.na(!!sample_col) & !is.na(!!mutgene_col)) %>%
        dplyr::mutate(row_col_ex_weights = edge_sum / !!Q) %>%
        dplyr::select(!!sample_col, !!mutgene_col, row_col_ex_weights)
    return(tib)
}


# make the sample-gene bipartite graph
make_sample_gene_biprartite <- function(s, g) {
    bgr <- tibble::tibble(samples = s, genes = g) %>%
        tidygraph::as_tbl_graph(directed = FALSE) %N>%
        tidygraph::mutate(type = name %in% s)
    return(bgr)
}


to_edgelist <- function(gr, col_names, names = TRUE) {
    el <- igraph::as_edgelist(gr, names = names)
    colnames(el) <- col_names
    el <- tibble::as_tibble(el) %>%
        dplyr::mutate(edge_sum = 1)
    return(el)
}

utils::globalVariables(c("edge_sum","row_col_ex_weights", "name"), add = TRUE)
