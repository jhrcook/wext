test_that("the RC-test works", {
    library(dplyr)
    dat <- tibble::tribble(
        ~samples, ~genes,
        "A", "gene1",
        "A", "gene2",
        "B", "gene1",
        "B", "gene3",
        "C", "gene3"
    )
    k <- 2
    N_perms <- 6
    rc_res <- rc_test(dat, samples, genes,
                      k = k,
                      N_perms = N_perms,
                      min_mutex_events = 0)

    expect_true(tibble::is_tibble(rc_res))
    expect_true(ncol(rc_res) == 4)

    gene_sets <- rc_res$gene_sets
    lengths <- purrr::map_lgl(gene_sets, ~ length(.x) == 2)
    expect_true(all(lengths))

    expect_equal(unlist(rc_res$t_AM), c(1, 2, 3))

    pval_check <- rc_res %>%
        mutate(pval_check = (t_BM_ge / !!N_perms) == p_val) %>%
        pull(pval_check) %>%
        unlist()
    expect_true(all(pval_check))
})