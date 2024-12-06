Advent of Code 2024, Day 5
================
Skip Perry
December 2024

``` r
options(dplyr.summarize.inform = FALSE)
library(tidyverse)
```

``` r
rules <- 
  "data/day5a.txt" |> 
  readLines() |> 
  strsplit("\\|") |> 
  unlist() |> 
  as.numeric() |> 
  matrix(ncol = 2, byrow = TRUE)
```

``` r
updates <- 
  "data/day5b.txt" |> 
  readLines() |> 
  strsplit(",") |> 
  lapply(as.numeric)
```

``` r
find_relevant_rules <- function(update) {
  rules[rules[,1] %in% update & rules[,2] %in% update,]
}

order_pages <- function(i) {
  rules <- 
    updates[[i]] |> 
    find_relevant_rules()
  
  pages <- unique(c(rules[,1], rules[,2]))
  page_order <- c()
  rules_tmp <- rules
  
  for (i in 1:(length(pages)-2)) {
    page_order[i] <- unique(rules_tmp[,1][which(rules_tmp[,1] %in% rules_tmp[,2] == FALSE)])
    rules_tmp <- rules_tmp[rules_tmp[,1] != page_order[i],]
  }
  
  c(page_order, rules_tmp)
}
```

``` r
check_order <- function(i) { all(order_pages(i) == updates[[i]]) }

middle_index <- function(vec) { vec[(length(vec) + 1) / 2] }

correctly_ordered <- 
  1:length(updates) |> 
  map_lgl(check_order)

updates[correctly_ordered] |>
  lapply(middle_index) |> 
  unlist() |> 
  sum()
```

    ## [1] 5964

``` r
middle_index_proper_order <- function(i) {
  i |> 
    order_pages() |> 
    middle_index()
}

which(!correctly_ordered) |> 
  map_int(middle_index_proper_order) |> 
  sum()
```

    ## [1] 4719
