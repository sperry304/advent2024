Advent of Code 2024, Day 9
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
  "data/day9a.txt" |> 
  readLines() |> 
  strsplit(split = "") |> 
  unlist() |> 
  as.numeric()

num_block <- function(idx, val) { rep(as.character(idx - 1), val) }
free_block <- function(idx, val) { rep(".", val) }

odd_entries <- dat[seq(1, length(dat), by = 2)]
even_entries <- c(dat[seq(2, length(dat) - 1, by = 2)], 0)

vec <-
  rbind(
    map2(1:length(odd_entries), odd_entries, num_block),
    map2(1:length(even_entries), even_entries, free_block)
  ) |> 
  c() |> 
  unlist()
```

``` r
vec_tmp <- vec
while(sum(vec_tmp == ".") > 0) {
  #if (length(vec_tmp) %% 1000 == 0) { print(length(vec_tmp)) }
  v <- vec_tmp[length(vec_tmp)]
  if (v != ".") { vec_tmp[min(which(vec_tmp == "."))] <- v}
  vec_tmp <- vec_tmp[-length(vec_tmp)]
}
format(sum(as.numeric(vec_tmp) * 0:(length(vec_tmp) - 1)), scientific = FALSE)
```

    ## [1] "6384282079460"

``` r
v_match <- function(needle, haystack, nomatch = 0L) { 
  sieved <- which(haystack == needle[1L]) 
  for (i in seq.int(1L, length(needle) - 1L)) {
    sieved <- sieved[haystack[sieved + i] == needle[i + 1L]]
  }
  sieved
}

v_contains <- function(needle, haystack) {
  sieved <- which(haystack == needle[1L]) 
  for (i in seq.int(1L, length(needle) - 1L)) {
    sieved <- sieved[haystack[sieved + i] == needle[i + 1L]]
  }
  length(sieved) && !anyNA(sieved)
}
```

``` r
vec_tmp <- vec
vec_tmp[vec_tmp == "."] <- -1
vec_tmp <- as.integer(vec_tmp)
nums <- ((length(dat) - 1) / 2):0

for (i in 1:length(nums)) {
  idx_of_current_num <- which(vec_tmp == nums[i])
  len_of_current_num <- length(idx_of_current_num)
  
  if (len_of_current_num == 1 & min(which(vec_tmp == -1L)) < min(idx_of_current_num)) {
    vec_tmp[min(which(vec_tmp == -1L))] <- nums[i]
    vec_tmp[idx_of_current_num] <- -1
  } else if (v_contains(rep(-1L, len_of_current_num), vec_tmp[1:idx_of_current_num[1]])) {
    start_idx_to_replace <- min(v_match(rep(-1L, len_of_current_num), vec_tmp), na.rm = TRUE)
    vec_tmp[start_idx_to_replace:(start_idx_to_replace + len_of_current_num - 1)] <- nums[i]
    vec_tmp[idx_of_current_num] <- -1
  }
}

vec_for_calc <- vec_tmp
vec_for_calc[vec_for_calc == -1] <- 0

format(sum(as.numeric(vec_for_calc) * 0:(length(vec_for_calc) - 1)), scientific = FALSE)
```

    ## [1] "6408966547049"
