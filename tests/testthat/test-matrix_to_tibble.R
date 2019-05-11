test_that("a matrix is turned into a tibble", {
    set.seed(0)
    mat <- matrix(sample(c(0,1), 20, replace = TRUE), nrow = 4)
    colnames(mat) <- LETTERS[1:5]
    rownames(mat) <- letters[1:4]
    expect_true(tibble::is_tibble(matrix_to_tibble(mat)))
    expect_error(matrix_to_tibble(matrix()))
})
