test_that("correctly check node attributes", {
    library(dplyr)
    library(tidygraph)

    gr <- create_ring(5) %N>%
        mutate(name = LETTERS[1:5]) %E>%
        mutate(edge_attr = 1:5)
    expect_true(check_gr(gr, "name"))
    expect_error(check_gr(gr, "fake_attr"), regexp = "fake_attr not a node attribute in the graph")
    expect_error(check_gr(gr, "edge_attr"), regexp = "edge_attr not a node attribute in the graph")
})
