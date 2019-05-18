#' Get the indices of nodes after evaluating 'expr'
#'
#' @description Evalute the expression in \code{tidygraph::filter(expr)} and
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

# for "get_node_index"
utils::globalVariables(c(".node_idx"), add = TRUE)
