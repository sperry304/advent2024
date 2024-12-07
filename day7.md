Advent of Code 2024, Day 7
================
Skip Perry
December 2024

``` r
options(dplyr.summarize.inform = FALSE)
library(tidyverse)
library(glue)

paste_c <- function(t) { paste0(t, collapse = "") }
```

``` r
dat <- 
  "data/day7a.txt" |> 
  readLines()

# dat
```

``` r
goals <- 
  dat |> 
  str_extract("\\d+") |> 
  as.double()

nums <- 
  dat |> 
  str_remove("\\d+\\: ") |> 
  strsplit(" ") |> 
  lapply(as.double)
```

``` r
day7 <- function(iter) {
  #if (iter %% 10 == 0) { print(iter) }

  n <- nums[[iter]]
  g <- goals[iter]
  
  if (prod(n) < g) { return(FALSE) } 
  if (sum(n) > g) { return(FALSE) } 

    ops_to_test <- 
    expand.grid(rep(list(c("+", "*")), length(n) - 1)) |> 
    mutate(across(-Var1, ~ str_c(")", .x)))
  
  n_df <- 
    n |> 
    matrix(ncol = ncol(ops_to_test) + 1, nrow = nrow(ops_to_test), byrow = TRUE) |> 
    as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
  
  results <- 
    cbind(n_df, ops_to_test)[,order(c(1:ncol(n_df), 1:ncol(ops_to_test)))] |> 
    add_column(paren = paste_c(rep("(", ncol(ops_to_test) - 1)), .before = 1L) |> 
    apply(1, paste_c) |> 
    sapply(function(x) eval(str2lang(x))) |> 
    as.numeric()
  
  any(results == g)
}

results <- map_lgl(1:length(goals), day7)

format(sum(goals[results]), scientific = FALSE)
```

    ## [1] "3066086663535"
