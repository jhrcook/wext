

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


test_that("swapping of a single edge works", {
    library(igraph)
    library(tidygraph)
    library(dplyr)

    # return a bolean vector of length(xs) for if each x in xs is even
    is_even <- function(xs) purrr::map_lgl(xs, .is_even)
    .is_even <- function(x) x %% 2 == 0

    # check that only one of the nodes for each edge is even
    check_edges <- function(nodes) {
        xor(is_even(nodes[[1]]), is_even(nodes[[2]]))
    }

    gr <- create_ring(10) %>%
        mutate(.idx = 1:n(),
               name = c("A", "b", "C", "d", "E", "f", "G", "h", "I", "j"))
    for (i in 1:100) {
        set.seed(i)
        swap_gr <- swap_an_edge(gr, seq(1, 9, 2), seq(2, 10, 2))
        expect_true(vcount(swap_gr) == vcount(gr))
        expect_true(ecount(swap_gr) == ecount(gr))
        el <- igraph::as_edgelist(gr, names = FALSE)
        checks <- apply(el, 1, check_edges)
        expect_true(all(checks))
    }
})

test_that("properly swap edges of a bipartite graph", {
    library(igraph)
    library(tidygraph)

    gr <- as_tbl_graph(simple_dataset, directed = FALSE)
    expect_error(bipartite_edge_swap(gr, Q = 100), regexp = "not a node attribute")
    gr <- gr %N>%
        mutate(type = name %in% unlist(simple_dataset$sample_name))

    swapped_gr <- bipartite_edge_swap(gr, Q = 10)

    expect_true(vcount(gr) == vcount(swapped_gr))
    expect_true(ecount(gr) == ecount(swapped_gr))

    el <- as_edgelist(swapped_gr, names = TRUE)

    # {boolean} is x a digit
    is_digit <- function(x) stringr::str_detect(x, "[:digit:]")
    # {boolean} is x a letter
    is_letter <- function(x) stringr::str_detect(x, "[:alpha:]")

    check_edge <- function(nodes) {
        if (is_digit(nodes[[1]]) & is_letter(nodes[[2]])) {
            return(TRUE)
        } else if (is_digit(nodes[[2]]) & is_letter(nodes[[1]])) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }

    checks <- apply(el, 1, check_edge)
    expect_true(all(checks))
})
