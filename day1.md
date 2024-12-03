Advent of Code 2024, Day 1
================
Skip Perry
December 2024

``` r
options(dplyr.summarize.inform = FALSE)
library(tidyverse)
```

``` r
day1a_test <- 
  "data/day1a_test.txt" |> 
  readLines() |> 
  as_tibble() |> 
  separate(value, into = c("a", "b"), sep = "  ") |> 
  mutate(across(everything(), as.numeric))

sum(sort(day1a_test$b) - sort(day1a_test$a))
```

    ## [1] 11

``` r
day1a <- 
  "data/day1a.txt" |> 
  readLines() |> 
  as_tibble() |> 
  separate(value, into = c("a", "b"), sep = "  ") |> 
  mutate(across(everything(), as.numeric))

sum(abs(sort(day1a$b) - sort(day1a$a)))
```

    ## [1] 1258579

``` r
day1a_test |> 
  count(num = a) |> 
  left_join(day1a_test |> count(num = b), by = "num") |> 
  filter(!is.na(n.y)) |> 
  summarize(similarity = sum(num * n.x * n.y))
```

    ## # A tibble: 1 × 1
    ##   similarity
    ##        <dbl>
    ## 1         31

``` r
day1a |> 
  count(num = a) |> 
  left_join(day1a |> count(num = b), by = "num") |> 
  filter(!is.na(n.y)) |> 
  summarize(similarity = sum(num * n.x * n.y))
```

    ## # A tibble: 1 × 1
    ##   similarity
    ##        <dbl>
    ## 1   23981443
