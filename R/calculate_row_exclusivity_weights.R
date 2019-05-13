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
    sample_col <- rlang::enquo(sample_col)
    mutgene_col <- rlang::enquo(mutgene_col)

    # number of samples
    n <- rlang::eval_tidy(sample_col, dat) %>%
        unlist() %>%
        unique() %>%
        length()

    if (n < 2) stop("Not enough unique samples to compare.")

    # calculate weight for Row-Exclusivity test
    new_dat <- dat %>%
        dplyr::group_by(!!mutgene_col) %>%
        dplyr::mutate(row_ex_weights = dplyr::n_distinct(!!sample_col) / !!n)

    return(new_dat)
}
