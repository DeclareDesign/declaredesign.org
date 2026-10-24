# Draw multivariate random variables

A formula interface for functions that return a matrix of draws (e.g.
[`MASS::mvrnorm`](https://rdrr.io/pkg/MASS/man/mvrnorm.html),
`extraDistr::rmnom`). The left-hand side of the formula names the
resulting columns; the right-hand side is any expression that returns an
N-by-k matrix.

## Usage

``` r
draw_multivariate(formula, sep = "_")
```

## Arguments

- formula:

  A two-sided formula. LHS: either a single bare name (used as a column
  prefix, producing `name_1`, `name_2`, ...) or a `c(name1, name2, ...)`
  call giving explicit column names. RHS: any expression returning an
  N-by-k numeric matrix.

- sep:

  Separator between prefix and index when LHS is a single name. Default
  `"_"`.

## Value

A tibble with one column per name on the left-hand side of the formula,
in that order, and one row per row of the matrix the right-hand side
returns.

## Details

When called inside
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
without a name on the left of `=`, the resulting columns are appended
directly to the data frame.

## Examples

``` r
if (requireNamespace("MASS", quietly = TRUE)) {
  S <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
  draw_multivariate(c(Y1, Y2) ~ MASS::mvrnorm(n = 100, mu = c(0, 0), Sigma = S))
}
#> # A tibble: 100 × 2
#>         Y1     Y2
#>      <dbl>  <dbl>
#>  1 -1.08    0.440
#>  2 -1.13    0.241
#>  3  0.168   0.737
#>  4  0.0344  1.05 
#>  5  0.883   0.850
#>  6  0.178  -1.21 
#>  7  0.122   1.50 
#>  8  0.635   0.847
#>  9 -0.338   0.274
#> 10 -1.60   -1.69 
#> # ℹ 90 more rows

# Inside fabricate (unnamed -> columns appended automatically)
if (requireNamespace("MASS", quietly = TRUE)) {
  S <- matrix(c(1, 0.4, 0.4, 1), 2, 2)
  fabricate(N = 50,
            draw_multivariate(c(X, Y) ~ MASS::mvrnorm(n = N, mu = c(0, 0), Sigma = S)))
}
#> # A tibble: 50 × 3
#>    ID           X       Y
#>    <chr>    <dbl>   <dbl>
#>  1 01    -0.0346   0.161 
#>  2 02    -0.403   -1.72  
#>  3 03     0.323   -0.850 
#>  4 04    -0.345    0.0533
#>  5 05    -0.00933 -0.832 
#>  6 06     0.799   -1.64  
#>  7 07     1.39     0.145 
#>  8 08    -0.312   -0.292 
#>  9 09    -1.93    -0.383 
#> 10 10    -0.0373   1.26  
#> # ℹ 40 more rows
```
