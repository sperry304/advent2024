Advent of Code 2024, Day 1
================
Skip Perry
December 2024

``` r
options(dplyr.summarize.inform = FALSE)
library(tidyverse)
```

``` r
day1 <- 
  "data/day1a.txt" |> 
  readLines() |> 
  as_tibble() |> 
  separate(value, into = c("a", "b"), sep = "  ") |> 
  mutate(across(everything(), as.numeric))

sum(abs(sort(day1$b) - sort(day1$a)))
```

    ## [1] 1258579

``` r
day1 |> 
  count(num = a) |> 
  left_join(day1 |> count(num = b), by = "num") |> 
  summarize(similarity = sum(num * n.x * n.y, na.rm = TRUE))
```

    ## # A tibble: 1 × 1
    ##   similarity
    ##        <dbl>
    ## 1   23981443
