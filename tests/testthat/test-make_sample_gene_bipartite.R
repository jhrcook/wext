test_that("make a proper bipartite graph", {
    library(tidygraph)
    set.seed(0)

    S <- as.character(seq(2, 10, 2))
    G <- as.character(seq(1, 9, 2))
    s <- sample(S, 100, replace = TRUE)
    g <- sample(G, 100, replace = TRUE)

    bgr <- make_sample_gene_bipartite(s, g)

    expect_equal(igraph::vcount(bgr), 10)
    expect_equal(igraph::ecount(bgr), 100)

    onlysamples <- bgr %N>% filter(type)
    onlygenes <- bgr %N>% filter(!type)

    expect_equal(igraph::ecount(onlysamples), 0)
    expect_equal(igraph::vcount(onlysamples), 5)
    expect_equal(igraph::ecount(onlygenes), 0)
    expect_equal(igraph::vcount(onlygenes), 5)
})
