#' Calculate the weights for the Row-Exclusivity test
#'
#' @description The weight for each gene in the Row-Exclusivity test is the
#'   number of samples with a mutation in a gene divided by the number of
#'   samples.
#'
#' @param dat tibble with mutation information
#' @param sample_col column of samples names (quoted)
#' @param mutgene_col column of genes that are mutated (quoted)
#'
#' @examples
#' library(wext)
#' calculate_row_exclusivity_weights(dat = simple_dataset,
#'                                   sample_col = sample_name,
#'                                   mutgene_col = mutated_gene)
#'
#' @importFrom magrittr %>%
#' @export calculate_row_exclusivity_weights
calculate_row_exclusivity_weights <- function(dat, sample_col, mutgene_col) {
    original_colnames <- c(
        rlang::as_string(rlang::ensym(sample_col)),
        rlang::as_string(rlang::ensym(mutgene_col))
    )

    sample_col <- rlang::enquo(sample_col)
    mutgene_col <- rlang::enquo(mutgene_col)

    # number of samples
    n <- rlang::eval_tidy(sample_col, dat) %>%
        unlist() %>%
        dplyr::n_distinct()
    if (n < 2) stop("Not enough unique samples to compare.")

    # calculate weight for Row-Exclusivity test
    gene_weights <- dat %>%
        dplyr::group_by(!!mutgene_col) %>%
        dplyr::summarise(row_ex_weights = dplyr::n_distinct(!!sample_col) / !!n) %>%
        dplyr::ungroup() %>%
        dplyr::select(!!mutgene_col, row_ex_weights)

    # get all combinations of samples and mutated genes
    # append the gene weights to each row
    mod_dat <- add_missing_combinations(dat, !!sample_col, !!mutgene_col,
                                        col_names = original_colnames) %>%
        dplyr::left_join(gene_weights, by = original_colnames[[2]]) %>%
        dplyr::arrange(!!sample_col, !!mutgene_col)

    return(mod_dat)
}


# add in the sample-mutant gene combinations that aren't already there
add_missing_combinations <- function(dat, sample_col, mutgene_col, col_names) {
    sample_col <- rlang::enquo(sample_col)
    mutgene_col <- rlang::enquo(mutgene_col)

    current_combs <- get_current_combinations(dat, !!sample_col, !!mutgene_col)
    full_grid <- get_all_combinations(dat, !!sample_col, !!mutgene_col) %>%
        dplyr::filter(!(.comb %in% !!current_combs)) %>%
        dplyr::select(Var1, Var2)
    if (nrow(full_grid) == 0) {
        # no combinaitons to add -- return original data
        return(dat)
    }
    colnames(full_grid) <- col_names
    mod_dat <- dplyr::bind_rows(dat, full_grid)
    return(mod_dat)
}


# get the current (i.e. existing) combinations of samples and mutated genes
get_current_combinations <- function(dat, sample_col, mutgene_col) {
    sample_col <- rlang::enquo(sample_col)
    mutgene_col <- rlang::enquo(mutgene_col)

    combs <- dat %>%
        dplyr::mutate(.comb = paste0(!!sample_col, "_", !!mutgene_col)) %>%
        dplyr::pull(.comb) %>%
        unlist() %>%
        unique()
    return(combs)
}


# get all combinations of the samples and mutated genes
get_all_combinations <- function(dat, sample_col, mutgene_col) {
    sample_col <- rlang::enquo(sample_col)
    mutgene_col <- rlang::enquo(mutgene_col)

    ss <- rlang::eval_tidy(sample_col, dat) %>% unlist() %>% unique()
    ms <- rlang::eval_tidy(mutgene_col, dat) %>% unlist() %>% unique()
    all_combs <- expand.grid(ss, ms, stringsAsFactors = FALSE) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(.comb = paste0(Var1, "_", Var2))
    return(all_combs)
}


utils::globalVariables(c(".comb", "Var1", "Var2", "row_ex_weights"), add = TRUE)

