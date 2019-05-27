// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <algorithm>
#include <iterator>
#include <stdlib.h>
using namespace Rcpp;

// Swap a single edge (method 2)
//
// @description Swap an edge of a bipartite graph while maintaining the
//   partition
//
// @param el edge list composed of a list of two vectors, one for each group of
//   the bipartite graph
// @param n_edges how many edges in the graph; if nothing is passed, this value
//   will be measured from the \code{el}, but passing this value from a
//   pre-calculated variable can save time by not measuring on each edge swap
// @param max_try Number of times to try to find two edges to swap. If no
//   edges are found, the program will crash with the message "Unable to swap
//   edges"
//
// @return a tidygraph graph object with two edges swapped
//
// @examples
// set.seed(0)
// library(tidygraph)
// gr <- tidygraph::create_ring(6, directed = FALSE) %>%
//     mutate(.idx = 1:n(), name = c("A", "b", "C", "d", "E", "f"))
// swaped_gr <- swap_an_edge(gr, c(1,3,5), c(2,4,6))
// plot(bind_graphs(gr, swaped_gr))
//
// @export swap_an_edge3
// [[Rcpp::export]]
NumericVector swap_an_edge3(NumericVector n1, NumericVector n2, int n_edges, int max_try) {

    LogicalVector idx(n_edges);


    bool CHECK = true;
    for (int try_counter = 0; CHECK && try_counter < max_try; ++try_counter)
    {

        // int rand_e1 = rand() % n_edges;
        // int rand_n11 = n1[rand_e1];
        // int rand_n12 = n1[rand_e1];
        //
        // // get the nodes adjacent to the nodes of the first edge
        // NumericVector adj_n11;
        // NumericVector adj_n12;
        // for (int i = 0; i < n_edges; i++)
        // {
        //     if (n2[i] == rand_n12)
        //     {
        //         adj_n12.push_back(n1[i]);
        //     }
        //     if (n1[i] == rand_n11)
        //     {
        //         adj_n11.push_back(n2[i]);
        //     }
        // }
        //
        // for (int i = 0; i < n_edges; i++)
        // {
        //     if (std::any_of(adj_n11.begin(), adj_n11.end(), [](int j){return j == n2[i];}) || std::any_of(adj_n12.begin(), adj_n12.end(), [](int j){return j == n2[i];}))
        //     {
        //         idx[i] = true;
        //     }
        //     else
        //     {
        //         idx[i] = false;
        //     }
        // }
        //
        // if ( std::all_of(idx.begin(), idx.end(), [](bool i){return i == true;}) )
        // {
        //     CHECK = true;
        // }
        // else
        // {
        //     CHECK = false;
        // }

    }

    // if the loop was unable to find an edge in max_try attempts
    if (CHECK)
    {
        return 0;
    }

    return n1;
}
