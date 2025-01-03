Advent of Code 2024, Day 2
================
Skip Perry
December 2024

``` r
options(dplyr.summarize.inform = FALSE)
library(tidyverse)
```

``` r
dat <- 
  "data/day2a.txt" |> 
  readLines() |> 
  strsplit(" ")

dat <- lapply(dat, as.numeric)

safety_check <- function(report) {
  r <- report[1:(length(report) - 1)] - report[-1]

  if ((all(r > 0) | all(r < 0)) == FALSE | all(abs(r) < 4) == FALSE) { 
    return(FALSE)
  } else {
    return(TRUE)
  }
}

res <- map_lgl(dat, safety_check)
sum(res)
```

    ## [1] 299

``` r
safety_check_pop <- function(report) {
  for (j in 1:length(report)) {
    if (safety_check(report[-j]) == TRUE) { return(TRUE) }
  }
  return(FALSE)
}

safety_check2 <- function(report) {
  r <- report[1:(length(report) - 1)] - report[-1]

  if ((all(r > 0) | all(r < 0)) == FALSE | all(abs(r) < 4) == FALSE) { 
    return(safety_check_pop(report))
  } else {
    return(TRUE)
  }
}

res <- map_lgl(dat, safety_check2)
sum(res)
```

    ## [1] 364
