
test_that("the edge list is made correctly for a bipartite graph", {
    library(tidygraph)
    library(dplyr)
    library(tibble)

    empty_gr <- create_empty(n = 10) %N>%
        mutate(type = rep(c(TRUE, FALSE), 5),
               name = LETTERS[1:10])
    expect_error(to_bipartite_edgelist(empty_gr), regexp = "no edges")

    ring_gr <- create_ring(n = 20) %N>%
        mutate(type = rep(c(TRUE, FALSE), 10),
               name = LETTERS[1:20])
    ring_el <- to_bipartite_edgelist(ring_gr)
    expect_equal(length(ring_el), 2)
    expect_equal(length(ring_el$nodes1), igraph::ecount(ring_gr))
    expect_equal(length(ring_el$nodes2), igraph::ecount(ring_gr))
    expect_true(
        length(intersect(unlist(ring_el$nodes1), unlist(ring_el$nodes2))) == 0
    )
})

test_that("an edge list is converted back to a bipartite graph correctly", {
    library(tidygraph)
    library(dplyr)
    library(tibble)

    el <- list(c(1,3,5,7), c(2,4,6,8))
    gr <- edgelist_to_bipartite_graph(el)
    expect_true(is.tbl_graph(gr))
    expect_equal(igraph::ecount(gr), 4)
    expect_equal(igraph::vcount(gr), 8)
    expect_true(check_gr(gr, "name"))
    expect_true(check_gr(gr, "type"))


    el <- list(c("A", "A", "A", "B", "B"),
               c("E", "C", "D", "C", "D"))
    gr <- edgelist_to_bipartite_graph(el, 1)
    expect_equal(igraph::vcount(gr), 5)
    expect_equal(igraph::ecount(gr), 5)
    expect_true(check_gr(gr, "name"))
    expect_true(check_gr(gr, "type"))
    v1 <- gr %N>% filter(type) %>% as_tibble() %>% pull(name) %>% unlist()
    v2 <- gr %N>% filter(!type) %>% as_tibble() %>% pull(name) %>% unlist()
    expect_true(all(v1 %in% unlist(el[1])))
    expect_true(all(v2 %in% unlist(el[2])))

    gr <- edgelist_to_bipartite_graph(el, 2)
    v1 <- gr %N>% filter(type) %>% as_tibble() %>% pull(name) %>% unlist()
    v2 <- gr %N>% filter(!type) %>% as_tibble() %>% pull(name) %>% unlist()
    expect_true(all(v1 %in% unlist(el[2])))
    expect_true(all(v2 %in% unlist(el[1])))
})

test_that("swapping of a single edge works (method 2)", {
    library(igraph)
    library(tidygraph)
    library(dplyr)

    # return a bolean vector of length(xs) for if each x in xs is even
    is_even <- function(xs) purrr::map_lgl(xs, .is_even)
    .is_even <- function(x) x %% 2 == 0

    # check that only one of the nodes for each edge is even
    check_edges <- function(nodes) {
        set1 <- LETTERS[seq(1, 10, 2)]
        xor(nodes[[1]] %in% set1, nodes[[2]] %in% set1)
    }

    gr <- create_ring(10) %>%
        mutate(.idx = 1:n(),
               name = c("A", "b", "C", "d", "E", "f", "G", "h", "I", "j"),
               type = rep(c(TRUE, FALSE), 5))
    el <- wext::to_bipartite_edgelist(gr)
    for (i in 1:100) {
        set.seed(i)
        swap_el <- swap_an_edge2(el)
        expect_true(n_distinct(unlist(swap_el)) == vcount(gr))
        expect_true(length(swap_el$nodes1) == ecount(gr))
        expect_true(length(swap_el$nodes2) == ecount(gr))
        check_el <- tibble::tibble(n1 = swap_el$nodes1, n2 = swap_el$nodes2)
        checks <- apply(check_el, 1, check_edges)
        expect_true(all(checks))
    }
})

test_that("properly swap edges of a bipartite graph", {
    library(igraph)
    library(tidygraph)

    gr <- as_tbl_graph(simple_dataset, directed = FALSE)
    expect_error(bipartite_edge_swap2(gr, Q = 100), regexp = "not a node attribute")
    gr <- gr %N>%
        mutate(type = name %in% unlist(simple_dataset$sample_name))

    swapped_gr <- bipartite_edge_swap2(gr, Q = 100)

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
