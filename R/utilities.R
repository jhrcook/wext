#' Get the indices of nodes after evaluating 'expr'
#'
#' @description Evaluate the expression in \code{tidygraph::filter(expr)} and
#'   return the indices of the remaining nodes
#'
#' @param gr tidygraph graph object
#' @param expr an expression to evaluate in \code{tidygraph::filter(expr)}
#'
#' @return a vector of the indices of the nodes remaining after the filter
#'
#' @examples
#' suppressPackageStartupMessages(library(magrittr))
#' suppressPackageStartupMessages(library(tidygraph))
#' tidygraph::create_ring(5) %>%
#'     mutate(name = LETTERS[1:5]) %>%
#'     get_node_index(name != "B")
#'
#' @importFrom magrittr %>%
#' @export get_node_index
get_node_index <- function(gr, expr) {
    expr <- rlang::enquo(expr)
    idx <- tibble::as_tibble(gr, active = "nodes") %>%
        dplyr::mutate(.node_idx = 1:dplyr::n()) %>%
        dplyr::filter(!!expr) %>%
        dplyr::pull(.node_idx) %>%
        unlist() %>%
        unique()
    if (length(idx) == 0) {
        return(NULL)
    } else {
        return(idx)
    }
}


#' Check that a node attribute exists for a graph
#'
#' @description If the node attribute exists, \code{TRUE} is returned
#'   silently; otherwise the function throws an informative error.
#'
#' @param gr a tidygraph graph object
#' @param node_attr name of node attribute to check for
#'
#' @return (silently) TRUE or throws an error with the message: "<node_attr> not
#'   a node attribute in the graph"
#'
#' @examples
#' gr <- tidygraph::create_ring(5)
#' gr <- tidygraph::mutate(gr, new_attr = LETTERS[1:5])
#' check_result <- check_gr(gr, "new_attr")
#' print(check_result)
#'
#' @export check_gr
check_gr <- function(gr, node_attr) {
    node_attrs <- colnames(tidygraph::as_tibble(gr, active = "nodes"))
    if (!node_attr %in% node_attrs) {
        stop(paste(node_attr, "not a node attribute in the graph"))
    }
    invisible(TRUE)
}



# for "get_node_index"
utils::globalVariables(c(".node_idx"), add = TRUE)
