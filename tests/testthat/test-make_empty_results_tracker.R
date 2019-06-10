test_that("correctly make the combinations tibble", {
    test_genes <- letters[1:10]

    test_seed_genes <- c()
    for (k in seq(1, 4)) {
        tt <- make_combs_tibble(genes = test_genes, k = k, seed_genes = test_seed_genes)
        expect_equal(nrow(tt), choose(length(test_genes), k))
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))
    }

    test_seed_genes <- c("a")
    for (k in seq(1, 4)) {
        tt <- make_combs_tibble(genes = test_genes, k = k, seed_genes = test_seed_genes)
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))

        gene_sets_have_seed <- purrr::map_lgl(
            tt$gene_sets,
            ~ all(test_seed_genes %in% .x)
        )
    }

    test_seed_genes <- c("a", "b")
    for (k in seq(1, 4)) {
        tt <- make_combs_tibble(genes = test_genes, k = k, seed_genes = test_seed_genes)
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))

        gene_sets_have_seed <- purrr::map_lgl(
            tt$gene_sets,
            ~ all(test_seed_genes %in% .x)
        )
    }

})


test_that("co-mutation results tracker is made properly", {
    library(tidygraph)
    library(igraph)
    library(dplyr)

    test_bgr <- as_tbl_graph(simple_dataset) %>%
        mutate(type = name %in% letters)

    test_seed_genes <- c()
    for (k in seq(1, 4)) {
        tt <- comutation_results_tracker(
            test_bgr, k = k, seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        expect_equal(nrow(tt), choose(length(test_genes), k))
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))
    }


    test_seed_genes <- c("1")
    for (k in seq(1, 4)) {
        tt <- comutation_results_tracker(
            test_bgr, k = k, seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        tt <- make_combs_tibble(genes = test_genes, k = k, seed_genes = test_seed_genes)
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))

        gene_sets_have_seed <- purrr::map_lgl(
            tt$gene_sets,
            ~ all(test_seed_genes %in% .x)
        )
    }

    test_seed_genes <- c("1", "2")
    for (k in seq(1, 4)) {
        tt <- comutation_results_tracker(
            test_bgr, k = k, seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        tt <- make_combs_tibble(genes = test_genes, k = k, seed_genes = test_seed_genes)
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))

        gene_sets_have_seed <- purrr::map_lgl(
            tt$gene_sets,
            ~ all(test_seed_genes %in% .x)
        )
    }

})


test_that("mutual exclusivity results tracker is made properly", {
    library(tidygraph)
    library(igraph)
    library(dplyr)

    test_bgr <- as_tbl_graph(simple_dataset) %>%
        mutate(type = name %in% letters)

    test_seed_genes <- c()
    for (k in seq(1, 4)) {
        tt <- exclusivity_results_tracker(
            test_bgr, k = k, seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        expect_equal(nrow(tt), choose(length(test_genes), k))
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))
    }


    test_seed_genes <- c("1")
    for (k in seq(1, 4)) {
        tt <- exclusivity_results_tracker(
            test_bgr, k = k, seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        tt <- make_combs_tibble(genes = test_genes, k = k, seed_genes = test_seed_genes)
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))

        gene_sets_have_seed <- purrr::map_lgl(
            tt$gene_sets,
            ~ all(test_seed_genes %in% .x)
        )
    }

    test_seed_genes <- c("1", "2")
    for (k in seq(1, 4)) {
        tt <- exclusivity_results_tracker(
            test_bgr, k = k, seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        tt <- make_combs_tibble(genes = test_genes, k = k, seed_genes = test_seed_genes)
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))

        gene_sets_have_seed <- purrr::map_lgl(
            tt$gene_sets,
            ~ all(test_seed_genes %in% .x)
        )
    }

})


test_that("properly make the empty results tracker for either test", {
    library(tidygraph)
    library(igraph)
    library(dplyr)

    test_bgr <- as_tbl_graph(simple_dataset) %>%
        mutate(type = name %in% letters)

    expect_error(make_empty_results_tracker(
            test_bgr, k = k, which_test = "not_a_test", seed_genes = test_seed_genes, min_times_mut = 0
        ),
        regexp = "not a choice of test")

    test_seed_genes <- c()
    for (k in seq(1, 4)) {
        tt <- make_empty_results_tracker(
            test_bgr, k = k, which_test = "e", seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        expect_equal(nrow(tt), choose(length(test_genes), k))
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))
    }


    test_seed_genes <- c("1")
    for (k in seq(1, 4)) {
        tt <- make_empty_results_tracker(
            test_bgr, k = k, which_test = "e", seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        tt <- make_combs_tibble(genes = test_genes, k = k, seed_genes = test_seed_genes)
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))

        gene_sets_have_seed <- purrr::map_lgl(
            tt$gene_sets,
            ~ all(test_seed_genes %in% .x)
        )
    }

    test_seed_genes <- c("1", "2")
    for (k in seq(1, 4)) {
        tt <- make_empty_results_tracker(
            test_bgr, k = k, which_test = "e", seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        tt <- make_combs_tibble(genes = test_genes, k = k, seed_genes = test_seed_genes)
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))

        gene_sets_have_seed <- purrr::map_lgl(
            tt$gene_sets,
            ~ all(test_seed_genes %in% .x)
        )
    }

    # for co-mutation
    test_seed_genes <- c()
    for (k in seq(1, 4)) {
        tt <- make_empty_results_tracker(
            test_bgr, k = k, which_test = "c", seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        expect_equal(nrow(tt), choose(length(test_genes), k))
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))
    }


    test_seed_genes <- c("1")
    for (k in seq(1, 4)) {
        tt <- make_empty_results_tracker(
            test_bgr, k = k, which_test = "c", seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        tt <- make_combs_tibble(genes = test_genes, k = k, seed_genes = test_seed_genes)
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))

        gene_sets_have_seed <- purrr::map_lgl(
            tt$gene_sets,
            ~ all(test_seed_genes %in% .x)
        )
    }

    test_seed_genes <- c("1", "2")
    for (k in seq(1, 4)) {
        tt <- make_empty_results_tracker(
            test_bgr, k = k, which_test = "c", seed_genes = test_seed_genes, min_times_mut = 0
        )

        test_genes <- as_tibble(test_bgr, active = "nodes") %>%
            filter(!type) %>%
            pull(name) %>%
            unlist()

        tt <- make_combs_tibble(genes = test_genes, k = k, seed_genes = test_seed_genes)
        expect_equal(ncol(tt), 2)
        expect_equal(colnames(tt), c("gene_sets", "t_BM_ge"))
        geneset_lengths <- purrr::map_dbl(tt$gene_sets, length)
        expect_true(all(geneset_lengths == k))
        expect_true(all(tt$t_BM_ge == 0))

        gene_sets_have_seed <- purrr::map_lgl(
            tt$gene_sets,
            ~ all(test_seed_genes %in% .x)
        )
    }

})

