#' Calculate the weights for the Weighted-Row-Exclusivity test
#'
#' @description The weighted test for mutual exclusivity aims to calculate the
#'   probability of observing at least \eqn{t_M} mutually exclusive events given
#'   a fixed number of samples (row-sums) and a per-gene, per-sample weight
#'   matrix \eqn{W}. For further explanation, see the "Computing the Weighted
#'   Exclusivity Test (WExT)" vignette. This function creates the weight matrix
#'   for the test.
#'
#' @param dat tibble with mutation information
#' @param sample_col column of samples names (quoted)
#' @param mutgene_col column of genes that are mutated (quoted)
#'
#' @examples
#' library(wext)
#' calculate_weighted_row_exclusivity_weights(dat = simple_dataset,
#'                                            sample_col = sample_name,
#'                                            mutgene_col = mutated_gene)
#'
#' @importFrom magrittr %>%
#' @importFrom tidygraph %N>%
#' @export calculate_weighted_row_exclusivity_weights
calculate_weighted_row_exclusivity_weights <- function(dat,
                                                       sample_col,
                                                       mutgene_col) {
    # get original column names to use later
    original_colnames <- c(
        rlang::as_string(rlang::ensym(sample_col)),
        rlang::as_string(rlang::ensym(mutgene_col))
    )

    # enquote the input column names
    sample_col <- rlang::enquo(sample_col)
    mutgene_col <- rlang::enquo(mutgene_col)

    # number of unique samples
    n <- rlang::eval_tidy(sample_col, dat) %>%
        unlist() %>%
        dplyr::n_distinct()
    if (n < 2) stop("Not enough unique samples to compare.")

    return("FUNCTION IN PROGRESS")
}
