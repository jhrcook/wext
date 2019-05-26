# wext (development version)

# wext 0.0.0.9002

* Implemented a much faster form of the bipartite edge swapping algorithm. The new method is vector based instead of manipulating the 'tidygraph' graph object through the 'igraph' and 'tidygraph' APIs. I did a quick comparison of the new and original methods in the vignette "Comparing Edge Swap Algorithms."

# wext 0.0.0.9001

* Created vignette explaining the statistics of WExT and algorithm for the Row-Column Exclusivity Test and Weighted-Exclusivity Test.
* A functional form of the Row-Column Exclusivity Test was created and tested.

# wext 0.0.0.9000

* Initialized the package in RStudio using 'devtools', 'usethis', 'pakrat', and 'pkgdown'. Continuous integration will be used via AppVeyor and Travis-CI.
