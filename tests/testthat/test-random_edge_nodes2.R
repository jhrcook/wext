test_that("a random edge is selected (#2)", {
    library(igraph)
    library(tidygraph)
    library(dplyr)


    gr <- create_ring(6) %N>%
        mutate(.idx = 1:n(),
               type = .idx %% 2 == 0,
               name = LETTERS[1:6])
    for (i in 1:50) {
        edges <- random_edge_nodes2(gr)

        expect_equal(length(edges$e1), 2)
        expect_equal(length(edges$e2), 2)

        expect_true(!any(edges$e1 %in% edges$e2))
        expect_true(!any(edges$e2 %in% edges$e1))
        expect_true(length(intersect(edges$e1, edges$e2)) == 0)
    }
})
