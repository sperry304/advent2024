Advent of Code 2024, Day 4
================
Skip Perry
December 2024

``` r
options(dplyr.summarize.inform = FALSE)
library(tidyverse)
```

``` r
dat <- 
  "data/day4a.txt" |> 
  readLines()

mat <- 
  dat |> 
  paste0(collapse = "") |> 
  strsplit(split = "") |> 
  unlist() |> 
  matrix(nrow = length(dat), byrow = TRUE)

dim_mat <- nrow(mat)

mat_rev <- mat[,dim_mat:1]
```

``` r
vert_horiz_count <- function(i) {
  mat[i,] |> paste0(collapse = "") |> str_count("XMAS") + 
  mat[i,] |> paste0(collapse = "") |> str_count("SAMX") + 
  t(mat)[i,] |> paste0(collapse = "") |> str_count("XMAS") + 
  t(mat)[i,] |> paste0(collapse = "") |> str_count("SAMX")
}

pull_diag <- function(i, j, mat) {
  txt <- paste0(c(mat[i, j], mat[i+1, j+1], mat[i+2, j+2], mat[i+3, j+3]), collapse = "")
  if (txt %in% c("XMAS", "SAMX")) { return(1L) } else { return(0L) }
}

diag_count <- 0

for (i in 1:(dim_mat-3)) {
  for (j in 1:(dim_mat-3)) {
    diag_count <- 
      diag_count + pull_diag(i, j, mat) + pull_diag(i, j, mat_rev)
  }
}

sum(map_int(1:dim_mat, vert_horiz_count)) + diag_count
```

    ## [1] 2530

``` r
num_exes <- 0

for (i in 2:(dim_mat-1)) { 
  for (j in 2:(dim_mat-1)) {
    if (mat[i, j] == "A") {
      x <- paste0(mat[i-1, j-1], mat[i-1, j+1], mat[i+1, j-1], mat[i+1, j+1], collapse = "")
      if (x %in% c("MMSS", "MSMS", "SMSM", "SSMM")) { num_exes <- num_exes + 1 }
    }
  }
}

num_exes
```

    ## [1] 1921
