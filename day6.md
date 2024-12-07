Advent of Code 2024, Day 6
================
Skip Perry
December 2024

``` r
options(dplyr.summarize.inform = FALSE)
library(tidyverse)

paste_c <- function(t) { paste0(t, collapse = "") }

dat_to_mat <- function(dat) {
  dat |> 
    paste_c() |> 
    strsplit(split = "") |> 
    unlist() |> 
    matrix(nrow = length(dat), byrow = TRUE)
}
```

``` r
dat <- 
  "data/day6a.txt" |> 
  readLines()

#dat
```

``` r
mat <- 
  dat |> 
  dat_to_mat()
```

``` r
check_next <- function(row, col, m) {
  if (row == 1) { return("done") }
  if (m[row-1, col] == "#") { return("blocked") }
  else { return("up") }
}
```

``` r
m <- mat

while (sum(m == "^") > 0) {
  curr_pos <- which(m == "^", arr.ind = TRUE)
  i <- curr_pos[1]; j <- curr_pos[2]; next_pos <- check_next(i, j, m)
  if (next_pos == "done") { m[i, j] <- "X"; break }
  else if (next_pos == "up") { m[i, j] <- "X"; m[i-1, j] <- "^" }
  else if (next_pos == "blocked") { m <- apply(t(m), 2, rev) }
}

sum(m == "X")
```

    ## [1] 4982

``` r
dat <- 
  "data/day6a.txt" |> 
  readLines()

#dat
```

``` r
mat <- 
  dat |> 
  dat_to_mat()

#mat
```

``` r
num_obstructions <- 0
starting_pos <- which(mat == "^", arr.ind = TRUE)
visited <- which(m == "X", arr.ind = TRUE)
length(visited)
dim(mat)

for (v in 1:nrow(visited)) {
  if (v %% 100 == 0) { print(v) }
  
  k <- as.numeric(visited[v,][1]); l <- as.numeric(visited[v,][2])
  if (k == starting_pos[1] & l == starting_pos[2]) { next }
  
  m <- mat
  m[k, l] <- "#"
  m2 <- matrix(0, nrow = nrow(m), ncol = ncol(m))
  
  while ((sum(m == "^") > 0) & (sum(m2 > 5) < 4)) {
    curr_pos <- which(m == "^", arr.ind = TRUE)
    i <- curr_pos[1]; j <- curr_pos[2]; next_pos <- check_next(i, j, m)
    if (next_pos == "done") { m[i, j] <- "X"; break }
    else if (next_pos == "up") { m[i, j] <- "X"; m[i-1, j] <- "^"; m2[i, j] <- m2[i, j] + 1 }
    else if (next_pos == "blocked") { m <- apply(t(m), 2, rev); m2 <- apply(t(m2), 2, rev) }
  }
  
  if (sum(m2 > 5) >= 4) { num_obstructions <- num_obstructions + 1 }
}

num_obstructions
v
```
