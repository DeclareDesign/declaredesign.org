# Sample N rows from a Cartesian product with optional correlation

Draws N rows from the cross-product of the specified levels. When `rho`
or `sigma` is non-zero, row assignments are correlated via a Gaussian
copula so that units with high values on one level's variable tend to be
paired with units with high values on the other level's variable.

## Usage

``` r
link_levels(N, .by, ..., rho = 0, sigma = NULL)
```

## Arguments

- N:

  Number of rows to sample from the product.

- .by:

  Character vector of level names, usually two.

- ...:

  Additional column expressions evaluated after linking.

- rho:

  Scalar correlation of the Gaussian copula that links the levels' row
  assignments (default 0 = independent). It is not the realized rank
  correlation, which is smaller by the copula's own relation,
  \\(6/\pi)\arcsin(\rho/2)\\: `rho = 0.7` lands at 0.68 and `rho = 0.3`
  at 0.283. Ignored if `sigma` is provided. Comes after `...`, so a
  column called `r` or `s` is a column and not a partial match.

- sigma:

  Square correlation matrix (dimension = `length(.by)`).

## Value

A `fabricatr_level` object, meaningful only as a named argument to
`fabricate`. It contributes `N` rows drawn from the same product
`cross_levels` would build, with replacement, so a combination can
appear more than once and others not at all, and `N` may exceed the size
of the product. Columns are as for `cross_levels`.

## Details

The correlated draw takes one code path, so a given seed produces the
same data on every machine. fabricatr 1.x switches to `mvnfast::rmvn()`
when that package is installed, and passes it a core count, so its
numbers move when either changes. This is the second and last place
where 2.0 can give different numbers than 1.x, and it only differs when
`mvnfast` is installed.

## Examples

``` r
fabricate(
  primary   = declare_level(N = 20, p_quality = runif(N, 1, 10)),
  secondary = declare_level(N = 15, s_quality = runif(N, 1, 10)),
  students  = link_levels(
    N = 200, .by = c("primary", "secondary"), rho = 0.5,
    score = p_quality + s_quality + rnorm(N)
  )
)
#> # A tibble: 200 × 6
#>    primary p_quality secondary s_quality students score
#>    <chr>       <dbl> <chr>         <dbl> <chr>    <dbl>
#>  1 05           4.94 11             4.84 001      12.2 
#>  2 02           3.61 04             9.78 002      12.9 
#>  3 05           4.94 14             9.46 003      14.2 
#>  4 09           4.21 05             3.70 004       7.58
#>  5 03           4.12 02             6.25 005       9.44
#>  6 14           3.36 01             4.87 006       7.87
#>  7 19           2.14 09             8.83 007      12.7 
#>  8 05           4.94 08             9.97 008      15.6 
#>  9 15           3.73 10             1.65 009       3.25
#> 10 18           8.61 15             9.35 010      17.7 
#> # ℹ 190 more rows
```
