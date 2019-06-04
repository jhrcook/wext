
#include <Rcpp.h>
using namespace Rcpp;

//' Swap a single edge (method 2 in C++)
//'
//' @description Swap an edge of a bipartite graph while maintaining the
//'   bipartite nature. This function is not perfectly named as it runs more
//'   than a single edge swap (whereas the R implemented functions only run a
//'   single swap).
//'
//' @param n1,n2 the graph built as a set of two vectors with the nodes for each edge
//' @param N the number of edges to swap (one at a time)
//' @param max_try Number of times to try to find two edges to swap. No message
//'   is relayed for a single unsuccessful edge swap; instead a message at the
//'   end prints the number of successful edge swaps in total.
//'
//' @return \code{n1} with nodes swapped
//'
//' @export swap_an_edgeC
// [[Rcpp::export]]
IntegerVector swap_an_edgeC(IntegerVector n1, IntegerVector n2, int N, int max_try) {

    int n_edges = n1.size();
    bool CHECKER = true;
    LogicalVector idx(n_edges, true);

    int rand_e1 = 0;
    int rand_n11 = 0;
    int rand_n12 = 0;

    int edge_swap_counter = 0;

    // do N edge swaps
    for (int i=0; i<N; i++) {

        // get an edge to swap that has other swapping partners
        for (int j=0; j<max_try; j++) {
            // get first edge and its nodes to swap
            rand_e1 = rand() % n_edges;
            rand_n11 = n1[rand_e1];
            rand_n12 = n2[rand_e1];

            // get other nodes adjacent to the edge selected
            IntegerVector adj_n12 = unique(ifelse(n2 == rand_n12, n1, 0));
            IntegerVector adj_n11 = unique(ifelse(n1 == rand_n11, n2, 0));

            // which edges can be chosen to swap with (false == can be swapped with)
            idx = in(n1, adj_n12) | in(n2, adj_n11);

            // if all idx is true --> no other edges to swap with; try again
            CHECKER = is_true(all(idx));
            if (!CHECKER) {
                break;
            }
        }

        if (!CHECKER) {
            edge_swap_counter++;

            // number of available edges to swap with
            int num_available_edges = 0;
            for (int k=0; k<n_edges; k++) {
                if (idx[k] == false) {
                    num_available_edges++;
                }
            }

            // vector of other edges to swap with
            IntegerVector available_edges(num_available_edges);
            int available_edges_counter = 0;
            for (int k=0; k<n_edges; k++) {
                if (idx[k] == false) {
                    available_edges[available_edges_counter] = k;
                    available_edges_counter++;
                }
            }

            // edge swap
            int rand_e2 = sample(available_edges, 1)[0];
            n1[rand_e1] = n1[rand_e2];
            n1[rand_e2] = rand_n11;

            Rcpp::checkUserInterrupt();
            CHECKER = true;
        }
    }

    Rcout << "number of successful edge swaps: " << edge_swap_counter << std::endl;

    return n1;
}
