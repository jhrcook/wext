## code to prepare `simple_dataset` dataset goes here

library(dplyr)
library(stringr)
library(tibble)

# split a line from the data set and make a tibble of the data
read_line <- function(x) {
    y <- str_split(x, "\\t") %>% unlist()
    tibble(sample_name = y[[1]],
           mutated_gene = unlist(y[-1]))
}

# path to simple example data set from the Raphael group
simple_dataset_path <- "raphael_examples/simple/adjacency_list.tsv"

# read each line, parse into tidy tibbles, bind into a single tibble
simple_dataset <- readLines(simple_dataset_path) %>%
    map(read_line) %>%
    bind_rows()

# save the data for use in the package
usethis::use_data(simple_dataset)
