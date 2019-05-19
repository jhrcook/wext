test_that("the Row-Column-Exclusivity weighting procedure works", {
    library(dplyr)

    empty_tib <- tibble::tibble(samples = "A", genes = "b")
    expect_error(calculate_row_col_exclusivity_weights(empty_tib, samples, genes),
                 regexp = "Not enough unique samples to compare.")

    dat <- tibble::tribble(
        ~samples, ~genes,
             "1",    "A",
             "1",    "B",
             "2",    "C",
             "2",    "B",
             "3",    "D",
             "3",    "A"
    )

    wrc <- calculate_row_col_exclusivity_weights(dat, samples, genes, Q = 10)

    col_sums_original <- dat %>%
        count(samples)
    col_sums_weight <- wrc %>%
        group_by(samples) %>%
        summarise(n = sum(row_col_ex_weights)) %>%
        ungroup()
    for (s in unique(dat$samples)) {
        r <- col_sums_original %>% filter(samples == s) %>% pull(n) %>% unlist()
        w <- col_sums_weight %>% filter(samples == s) %>% pull(n) %>% unlist()
        expect_equal(w, r)
    }

    row_sums_original <- dat %>%
        count(genes)
    row_sums_weight <- wrc %>%
        group_by(genes) %>%
        summarise(n = sum(row_col_ex_weights)) %>%
        ungroup()
    for (g in unique(dat$genes)) {
        r <- row_sums_original %>% filter(genes == g) %>% pull(n) %>% unlist()
        w <- row_sums_weight %>% filter(genes == g) %>% pull(n) %>% unlist()
        expect_equal(w, r)
    }
})
