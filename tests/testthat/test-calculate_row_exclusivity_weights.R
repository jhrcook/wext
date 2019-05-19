test_that("The Row-Exclusivity weights are calculated correctly", {
    library(dplyr)

    empty_tib <- tibble::tibble(x = "A", y = "b")
    expect_error(calculate_row_exclusivity_weights(empty_tib, x, y),
                 regexp = "Not enough unique samples to compare.")

    empty_dat <- tibble::tibble(samples = NA, genes = NA) %>%
        dplyr::slice(0)
    expect_error(calculate_row_exclusivity_weights(empty_wr, samples, genes))

    dat <- tibble::tribble(
        ~samples, ~genes,
             "1",    "A",
             "1",    "B",
             "2",    "C",
             "2",    "B",
             "3",    "D",
             "3",    "A"
    )
    wr <- calculate_row_exclusivity_weights(dat, samples, genes)

    # test individual values
    a_val <- wr %>% dplyr::filter(genes == "A") %>% dplyr::pull(row_ex_weights) %>% unlist() %>% unique()
    b_val <- wr %>% dplyr::filter(genes == "B") %>% dplyr::pull(row_ex_weights) %>% unlist() %>% unique()
    c_val <- wr %>% dplyr::filter(genes == "C") %>% dplyr::pull(row_ex_weights) %>% unlist() %>% unique()
    d_val <- wr %>% dplyr::filter(genes == "D") %>% dplyr::pull(row_ex_weights) %>% unlist() %>% unique()
    expect_equal(a_val, 2/3)
    expect_equal(b_val, 2/3)
    expect_equal(c_val, 1/3)
    expect_equal(d_val, 1/3)

    # test that the row sums are the same
    row_sums_original <- dat %>%
        count(genes)
    row_sums_weight <- wr %>%
        group_by(genes) %>%
        summarise(n = sum(row_ex_weights)) %>%
        ungroup()
    for (g in unique(dat$genes)) {
        r <- row_sums_original %>% filter(genes == g) %>% pull(n) %>% unlist()
        w <- row_sums_weight %>% filter(genes == g) %>% pull(n) %>% unlist()
        expect_equal(w, r)
    }
})
