Advent of Code 2024, Day 8
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
  "data/day8a.txt" |> 
  readLines()

mat <- 
  dat |> 
  dat_to_mat()

antenna_types <- 
  mat[mat != "."] |> 
  paste_c() |> 
  strsplit("") |> 
  unlist() |> 
  unique()

antenna_types
```

    ##  [1] "G" "U" "E" "W" "s" "f" "F" "9" "S" "Y" "v" "b" "3" "d" "p" "w" "7" "g" "C"
    ## [20] "8" "0" "V" "Q" "u" "B" "D" "6" "n" "1" "P" "q" "N" "k" "K" "a" "y" "c" "e"
    ## [39] "A" "2" "5" "4"

``` r
antinode_locations <- matrix(data = -1, nrow = 1, ncol = 2)

for (a in 1:length(antenna_types)) {
  antenna <- antenna_types[a]
  antenna_locations <- which(mat == antenna, arr.ind = TRUE)
  
  if (nrow(antenna_locations) == 1) { next }
  
  for (i in 1:(nrow(antenna_locations) - 1)) {
    for (j in (i+1):nrow(antenna_locations)) {
      loc_1 <- antenna_locations[i,]
      loc_2 <- antenna_locations[j,]
      
      antinode_locations <- 
        rbind(
          antinode_locations, 
          loc_1 + (loc_1 - loc_2),
          loc_2 - (loc_1 - loc_2)
        )
    }
  }
}

antinode_locations |> 
  as_tibble() |> 
  filter(row > 0, row <= nrow(mat), col > 0, col <= ncol(mat)) |> 
  distinct() |> 
  nrow()
```

    ## [1] 295

``` r
antinode_locations <- matrix(data = -1, nrow = 1, ncol = 2)

for (a in 1:length(antenna_types)) {
  antenna <- antenna_types[a]
  antenna_locations <- which(mat == antenna, arr.ind = TRUE)
  
  if (nrow(antenna_locations) == 1) { next }
  
  for (i in 1:(nrow(antenna_locations) - 1)) {
    for (j in (i+1):nrow(antenna_locations)) {
      loc_1 <- antenna_locations[i,]
      loc_2 <- antenna_locations[j,]
      
      loc_right <- loc_1 - (loc_1 - loc_2)
      while(all(loc_right <= nrow(mat) & loc_right > 0)) {
        antinode_locations <- rbind(antinode_locations, loc_right)
        loc_right <- loc_right - (loc_1 - loc_2)
      }
      
      loc_left <- loc_2 + (loc_1 - loc_2)
      while(all(loc_left <= nrow(mat) & loc_left > 0)) {
        antinode_locations <- rbind(antinode_locations, loc_left)
        loc_left <- loc_left + (loc_1 - loc_2)
      }
    }
  }
}

antinode_locations |> 
  rbind(antenna_locations) |> 
  as_tibble() |> 
  filter(row > 0, row <= nrow(mat), col > 0, col <= ncol(mat)) |> 
  distinct() |> 
  nrow()
```

    ## [1] 1034
