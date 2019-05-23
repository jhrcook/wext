
library(tibble)
library(dplyr)
library(tidygraph)

many_mutex <- tribble(
    ~samples, ~mut_genes,
        "s1", list("g1", "g3"),
        "s2", list("g1"),
        "s3", list("g2"),
        "s4", list("g3")
) %>%
    tidyr::unnest(mut_genes) %>%
    mutate(mut_genes = unlist(mut_genes))
many_mutex <- make_sample_gene_bipartite(
    many_mutex$samples,
    many_mutex$mut_genes
)

many_comut <- tribble(
    ~samples, ~mut_genes,
        "s1", list("g1", "g2"),
        "s2", list("g1", "g2", "g3"),
        "s3", list("g2", "g3"),
        "s4", list("g1", "g2", "g3")
) %>%
    tidyr::unnest(mut_genes) %>%
    mutate(mut_genes = unlist(mut_genes))
many_comut <- make_sample_gene_bipartite(
    many_comut$samples,
    many_comut$mut_genes
)

empty_gr <- create_empty(n = 10) %N>%
    mutate(name = c(paste0("s", 1:5), paste0("g", 1:5)),
           type = c(rep(TRUE, 5), rep(FALSE, 5)))

gene_sets <- list(
    list(gs = c("g1", "g2"), mx = 3, cm = 3),
    list(gs = c("g1", "g3"), mx = 2, cm = 2),
    list(gs = c("g2", "g3"), mx = 3, cm = 3)
)

test_that("correctly calculate the number of mutation events", {
    library(tidygraph)

    for (test_case in gene_sets) {
        expect_equal(calc_mutex_events(test_case$gs, many_mutex), test_case$mx)
        expect_equal(calc_comut_events(test_case$gs, many_comut), test_case$cm)
        expect_equal(calc_mutex_events(test_case$gs, empty_gr), 0)
        expect_equal(calc_comut_events(test_case$gs, empty_gr), 0)
    }

})
