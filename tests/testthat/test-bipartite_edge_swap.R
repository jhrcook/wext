

test_that("an edge is removed correctly", {
    library(igraph)
    library(tidygraph)

    gr <- create_ring(3)
    new_gr <- remove_edge(gr, 1, 2)
    expect_equal(ecount(new_gr), 2)
    expect_equal(vcount(new_gr), 3)
    expect_true(is_connected(new_gr))

    # removing an edge that doesn't exist
    new_new_gr <- remove_edge(new_gr, 1, 2)
    expect_equal(ecount(new_new_gr), 2)
    expect_equal(vcount(new_new_gr), 3)
    expect_true(is_connected(new_new_gr))
})


test_that("a random edge is selected", {
    library(igraph)
    library(tidygraph)
    library(dplyr)

    # failure with no ".idx" node attribute
    gr <- create_empty(5)
    expect_error(random_edge_nodes(gr), regexp = "not a node attribute")

    # failure with no edges
    gr <- gr %N>% mutate(.idx = 1:n())
    expect_error(random_edge_nodes(gr), regexp = "does not contain any edges")

    gr <- create_ring(5) %N>%
        mutate(.idx = 1:n())
    nodes <- random_edge_nodes(gr)
    expect_equal(length(nodes), 2)
    expect_true(nodes[[1]] != nodes[[2]])

    # test the targeted removal of nodes
    # looking at the graph, the removal of node 1 should only leave 3 and 4
    nodes <- random_edge_nodes(gr, ignore_nodes_connected_to = c(1)) %>%
        sort()
    expect_equal(nodes, c(3,4))
})


# test_that("swapping of a single edge works", {
#     library(tidygraph)
#     library(dplyr)
#
#     gr <- create_ring(10) %>%
#         mutate()
#
# })

# test_that("properly swap edges of a bipartite graph", {
#     library(tidygraph)
#     gr <- as_tbl_graph(simple_dataset, directed = FALSE) %N>%
#         mutate(type = name %in% unlist(simple_dataset$sample_name))
#
# })
