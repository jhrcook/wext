test_that("retrieve the node index of a graph", {
    library(tidygraph)
    quick_forestfire <- function(n) {
        play_forestfire(n, 0.5) %>% mutate(name = LETTERS[1:n])
    }
    expect_equal(get_node_index(quick_forestfire(10), name == "B"), 2)
    expect_null(get_node_index(quick_forestfire(10), name == "W"))
    expect_equal(get_node_index(quick_forestfire(10), name %in% c("B", "C", "D")), c(2, 3, 4))
    expect_equal(get_node_index(quick_forestfire(10), stringr::str_detect(name, "A|B|C")), c(1, 2, 3))
})
