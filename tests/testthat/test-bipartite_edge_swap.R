test_that("properly swap edges of a bipartite graph", {
    library(tidygraph)
    gr <- as_tbl_graph(simple_dataset, directed = FALSE) %N>%
        mutate(type = name %in% unlist(simple_dataset$sample_name))

})
