test_that("The Row-Exclusivity weights are calculated correctly", {
    empty_dat <- tibble::tibble(samples = NA, genes = NA) %>%
        dplyr::slice(0)
    expect_error(calculate_row_exclusivity_weights(empty_wr, samples, genes))

    dat <- tibble::tribble(
        ~samples, ~genes,
               1,    "A",
               1,    "B",
               2,    "C",
               2,    "B",
               3,    "D",
               3,    "A"
    )
    wr <- calculate_row_exclusivity_weights(dat, samples, genes)
    a_val <- wr %>% dplyr::filter(genes == "A") %>% dplyr::pull(row_ex_weights) %>% unlist() %>% unique()
    b_val <- wr %>% dplyr::filter(genes == "B") %>% dplyr::pull(row_ex_weights) %>% unlist() %>% unique()
    c_val <- wr %>% dplyr::filter(genes == "C") %>% dplyr::pull(row_ex_weights) %>% unlist() %>% unique()
    d_val <- wr %>% dplyr::filter(genes == "D") %>% dplyr::pull(row_ex_weights) %>% unlist() %>% unique()

    expect_equal(a_val, 2/3)
    expect_equal(b_val, 2/3)
    expect_equal(c_val, 1/3)
    expect_equal(d_val, 1/3)
})
