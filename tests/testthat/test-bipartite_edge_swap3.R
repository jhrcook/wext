
test_that("properly swap edges of a bipartite graph (method 3)", {
    library(igraph)
    library(tidygraph)

    gr <- as_tbl_graph(simple_dataset, directed = FALSE)
    expect_error(bipartite_edge_swap3(gr, Q = 100), regexp = "not a node attribute")
    gr <- gr %N>%
        mutate(type = name %in% unlist(simple_dataset$sample_name))

    # swapped_gr <- bipartite_edge_swap3(gr, Q = 100)
    swapped_gr <- bipartite_edge_swap3(gr, N = 10)

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
