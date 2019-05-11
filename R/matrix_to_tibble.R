#' Convert between a mutation matrix and tibble
#'
#' @description While tibbles are great for interactive data science, matrices
#'   are more practical for the algebra used by WExT. Therefore, these two
#'   functions \code{mutationmatrix_to_tibble()} and
#'   \code{tibble_to_mutationmatrix()}, make switching between the two formats
#'   much easier.
#'
#' @param dat a matrix object with column and row names
#' @param columns_name title for the data in the column names (default "sample_id")
#' @param rows_name title for the data in the row names (default "gene")
#'
#' @examples
#' set.seed(0)
#' mat <- matrix(sample(c(0,1), 12, replace = TRUE), nrow = 3)
#' colnames(mat) <- LETTERS[1:4]
#' rownames(mat) <- letters[1:3]
#' matrix_to_tibble(mat)
#'
#' @importFrom magrittr %>%
#' @export matrix_to_tibble
matrix_to_tibble <- function(dat, columns_name = "sample_id", rows_name = "gene") {
    check_dims(dat)  # check there are at least two rows and columns
    dat %>%
        tibble::as_tibble(rownames = rows_name) %>%
        tidyr::gather(key = columns_name, value = "mutated", -!!rows_name)
}

check_dims <- function(dat) {
    if (any(dim(dat) < 2)) {
        stop("Dimensions of matrix must be at least 2x2")
    } else {
        invisible(TRUE)
    }
}
