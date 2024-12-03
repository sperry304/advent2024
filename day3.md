Advent of Code 2024, Day 3
================
Skip Perry
December 2024

``` r
options(dplyr.summarize.inform = FALSE)
library(tidyverse)
```

``` r
dat <- 
  "data/day3a.txt" |> 
  readLines()

#dat
```

``` r
dat |> 
  paste0(collapse = "") |> 
  str_extract_all("mul\\(\\d+\\,\\d+\\)") |> 
  unlist() |> 
  as_tibble() |> 
  separate(value, into = c("a", "b"), sep = ",") |> 
  mutate(across(everything(), ~ as.numeric(str_extract(.x, "\\d+")))) |> 
  summarize(sum(a * b))
```

    ## # A tibble: 1 × 1
    ##   `sum(a * b)`
    ##          <dbl>
    ## 1    167090022

``` r
dat |> 
  paste0(collapse = "") |> 
  strsplit("do\\(\\)") |> 
  unlist() |> 
  str_remove("don't\\(\\)(.+)") |> 
  str_extract_all("mul\\(\\d+\\,\\d+\\)") |> 
  unlist() |> 
  as_tibble() |> 
  separate(value, into = c("a", "b"), sep = ",") |> 
  mutate(across(everything(), ~ as.numeric(str_extract(.x, "\\d+")))) |> 
  summarize(sum(a * b))
```

    ## # A tibble: 1 × 1
    ##   `sum(a * b)`
    ##          <dbl>
    ## 1     89823704
