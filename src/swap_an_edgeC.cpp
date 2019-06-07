
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
//' @param max_try number of times to try to find two edges to swap; no message
//'   is relayed for a single unsuccessful edge swap - instead a message at the
//'   end prints the number of successful edge swaps in total
//' @param quiet boolean for if you want a message if not \code{N} edge swaps
//'   are performed
//'
//' @return \code{n1} with nodes swapped
//'
//' @export swap_an_edgeC
// [[Rcpp::export]]
IntegerVector swap_an_edgeC(IntegerVector n1, IntegerVector n2, int N, int max_try, bool quiet) {

    // number of edges
    int n_edges = n1.size();

    // trackers
    int successes = 0;
    int counter = 0;
    int total_attempts = N + max_try;

    // calculate all random edges to use at one time (faster)
    IntegerVector random_edges = sample(n_edges, total_attempts, true) - 1;

    // perform all edge swaps or hit maximum fails
    while (successes < N & counter < total_attempts) {
        // Rcout << "Counter: " << counter << std::endl;
        // get first edge and its nodes to swap
        int rand_e1 = random_edges[counter];
        int rand_n11 = n1[rand_e1];
        int rand_n12 = n2[rand_e1];

        // get other nodes adjacent to the edge selected
        IntegerVector adj_n12 = unique(ifelse(n2 == rand_n12, n1, 0));
        IntegerVector adj_n11 = unique(ifelse(n1 == rand_n11, n2, 0));

        // which edges can be chosen to swap with (false == can be swapped with)
        LogicalVector idx = in(n1, adj_n12) | in(n2, adj_n11);

        // if all idx is true --> no other edges to swap with; try again
        if (!is_true(all(idx))) {
            // vector of other edges to swap with
            IntegerVector available_edges;
            for (int k=0; k<n_edges; k++) {
                if (idx[k] == false) {
                    available_edges.push_back(k);
                }
            }

            // edge swap
            int rand_e2 = sample(available_edges, 1)[0];
            n1[rand_e1] = n1[rand_e2];
            n1[rand_e2] = rand_n11;

            successes++;
        }
        counter++;

        // good-guy Rcpp programmer: checks for user interupts
        Rcpp::checkUserInterrupt();
    }

    // message about number of successful edge swaps
    if (quiet == false & successes != N) {
        Rcout << "Did not reach total number of edge swaps!\n --> only" << successes << "edges swapped" << std::endl;
    }

    // return the first edge list (the other was never modified)
    return n1;
}
