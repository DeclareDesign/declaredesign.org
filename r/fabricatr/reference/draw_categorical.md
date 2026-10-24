# Draw categorical outcomes

Draws from a multinomial distribution. Supply a probability matrix (one
row per observation, one column per category) or a single probability
vector shared across all observations.

## Usage

``` r
draw_categorical(prob, N = NULL, labels = NULL, category_labels = NULL)
```

## Arguments

- prob:

  Probability specification. Either:

  - A numeric vector of category probabilities (same for all
    observations; `N` must be supplied), or

  - A matrix with `N` rows and one column per category.

  Rows need not sum to 1; they are normalised internally.

- N:

  Number of observations. Required when `prob` is a vector.

- labels:

  Optional character vector of category labels (length must equal the
  number of categories). When supplied, returns a factor. The categories
  are nominal, so the factor is unordered, as it is in fabricatr 1.x.

- category_labels:

  fabricatr 1.x's name for `labels`. Accepted with a warning.

## Value

An integer vector of category indices `1`, `2`, and so on, one per row
of `prob`, or an unordered factor with those `labels` when they are
supplied.

## Examples

``` r
# Shared probabilities across all units
fabricate(N = 100, cat = draw_categorical(prob = c(0.2, 0.5, 0.3), N = N))
#> # A tibble: 100 × 2
#>    ID      cat
#>    <chr> <int>
#>  1 001       2
#>  2 002       3
#>  3 003       1
#>  4 004       2
#>  5 005       2
#>  6 006       2
#>  7 007       1
#>  8 008       2
#>  9 009       3
#> 10 010       2
#> # ℹ 90 more rows

# Per-unit probability matrix
fabricate(N = 4, p1 = runif(N), p2 = runif(N), p3 = runif(N),
          cat = draw_categorical(prob = cbind(p1, p2, p3)))
#> # A tibble: 4 × 5
#>   ID       p1     p2    p3   cat
#>   <chr> <dbl>  <dbl> <dbl> <int>
#> 1 1     0.268 0.0802 0.466     1
#> 2 2     0.485 0.180  0.892     1
#> 3 3     0.604 0.795  0.742     1
#> 4 4     0.213 0.529  0.764     3

# With labels -> factor
fabricate(N = 100,
          edu = draw_categorical(prob = c(0.3, 0.5, 0.2), N = N,
                                 labels = c("low", "medium", "high")))
#> # A tibble: 100 × 2
#>    ID    edu   
#>    <chr> <fct> 
#>  1 001   high  
#>  2 002   medium
#>  3 003   medium
#>  4 004   medium
#>  5 005   medium
#>  6 006   medium
#>  7 007   low   
#>  8 008   low   
#>  9 009   medium
#> 10 010   medium
#> # ℹ 90 more rows
```
