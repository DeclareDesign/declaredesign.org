# Modify columns of an existing level, or of the rows in hand

Adds or overwrites columns, as
[`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html)
does. Named after a level that already exists, it works at that level:
the expressions are evaluated once per unit of the level, `N` is the
number of units, and the result is written back to every row of each
unit. Left unnamed, it works on the rows in hand, with an optional `.by`
for split-apply-combine on a grouping column.

## Usage

``` r
modify_level(..., .by = NULL)
```

## Arguments

- ...:

  Column expressions to add or overwrite.

- .by:

  Optional character string: column name to group by before evaluating
  expressions.

## Value

A `fabricatr_level` object, meaningful only as a named or unnamed
argument to `fabricate`. It adds or overwrites columns and changes no
rows: the frame it returns has the same number of rows as the one it
received.

## Details

On a flat frame, unnamed `modify_level(..., .by = "g")` gives the same
columns as `group_by(g) |> mutate(...) |> ungroup()`. Its use is that it
runs inside the
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
call, so it is part of a DeclareDesign `declare_model()` step rather
than a pipe after it, and that only it can work at a named level.

## Which columns a level can see

Inside `regions = modify_level(...)`, the columns in view are the ones
that take a single value per region: those built at the regions level or
above it. A column built at a level nested inside regions varies within
a region, so it is out of view, as it is in fabricatr 1.x. To summarise
such a column per region, leave the level name off and group instead:
`modify_level(mean_b = mean(b), .by = "regions")`.

## Examples

``` r
# At a named level: one draw per region, written to every city in it
fabricate(
  regions = add_level(N = 3, a = 1:3),
  cities  = add_level(N = 2, b = rnorm(N)),
  regions = modify_level(z = rnorm(N), a2 = a * 2)
)
#> # A tibble: 6 × 6
#>   regions     a cities       b      z    a2
#>   <chr>   <int> <chr>    <dbl>  <dbl> <dbl>
#> 1 1           1 1       0.172   1.12      2
#> 2 1           1 2       1.72    1.12      2
#> 3 2           2 3       0.546  -1.46      4
#> 4 2           2 4      -0.107  -1.46      4
#> 5 3           3 5      -0.0303 -0.296     6
#> 6 3           3 6      -0.666  -0.296     6

# On the rows in hand, grouped
fabricate(
  N = 50,
  cluster = sample(1:5, N, replace = TRUE),
  Y = rnorm(N),
  modify_level(cluster_mean = mean(Y), .by = "cluster")
)
#> # A tibble: 50 × 4
#>    ID    cluster      Y cluster_mean
#>    <chr>   <int>  <dbl>        <dbl>
#>  1 01          2  0.162      -0.0480
#>  2 02          4 -0.196       0.188 
#>  3 03          2 -0.412      -0.0480
#>  4 04          2 -0.847      -0.0480
#>  5 05          4 -0.620       0.188 
#>  6 06          3  0.455       0.265 
#>  7 07          4 -0.313       0.188 
#>  8 08          5  1.28       -0.352 
#>  9 09          1 -0.619       0.362 
#> 10 10          2  0.253      -0.0480
#> # ℹ 40 more rows
```
