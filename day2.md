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
  l <- report
  
  descending <- all(l[1:(length(l) - 1)] - l[-1] > 0)
  ascending <- all(l[1:(length(l) - 1)] - l[-1] < 0)
  chg_3_max <- all(abs(l[1:(length(l) - 1)] - l[-1]) < 4)
  
  if (descending == FALSE & ascending == FALSE) { 
    return(FALSE)
  } else if (chg_3_max == FALSE) { 
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
  l <- report
  
  descending <- all(l[1:(length(l) - 1)] - l[-1] > 0)
  ascending <- all(l[1:(length(l) - 1)] - l[-1] < 0)
  chg_3_max <- all(abs(l[1:(length(l) - 1)] - l[-1]) < 4)
  
  if (descending == FALSE & ascending == FALSE) { 
    return(safety_check_pop(l))
  } else if (chg_3_max == FALSE) { 
    return(safety_check_pop(l))
  } else {
    return(TRUE)
  }
}

res <- map_lgl(dat, safety_check2)
sum(res)
```

    ## [1] 364
