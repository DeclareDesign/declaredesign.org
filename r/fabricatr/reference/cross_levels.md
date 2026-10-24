# Create a full Cartesian product of declared levels

Produces all combinations of the specified levels (equivalent to SQL
CROSS JOIN). Use `link_levels` to sample N rows from the product with an
optional correlation structure.

## Usage

``` r
cross_levels(.by, ...)
```

## Arguments

- .by:

  Character vector of level names to cross (must have been created by
  `add_level` or `declare_level` in the same `fabricate` call).

- ...:

  Additional column expressions evaluated after crossing.

## Value

A `fabricatr_level` object, meaningful only as a named argument to
`fabricate`. It contributes one row per combination of the levels named
in `.by`, so the frame has as many rows as their sizes multiplied
together, carrying every column of those levels plus an ID column of its
own. The first level in `.by` varies fastest.

## Examples

``` r
fabricate(
  countries = declare_level(N = 10, gdp = runif(N, 1, 10)),
  years     = declare_level(N = 5, shock = runif(N, 0, 1)),
  obs       = cross_levels(.by = c("countries", "years"), Y = gdp + shock)
)
#> # A tibble: 50 × 6
#>    countries   gdp years shock obs       Y
#>    <chr>     <dbl> <chr> <dbl> <chr> <dbl>
#>  1 01         8.88 1     0.546 01     9.42
#>  2 02         3.64 1     0.546 02     4.19
#>  3 03         3.02 1     0.546 03     3.57
#>  4 04         7.22 1     0.546 04     7.76
#>  5 05         3.81 1     0.546 05     4.36
#>  6 06         5.35 1     0.546 06     5.90
#>  7 07         2.10 1     0.546 07     2.64
#>  8 08         7.21 1     0.546 08     7.76
#>  9 09         3.84 1     0.546 09     4.39
#> 10 10         8.35 1     0.546 10     8.89
#> # ℹ 40 more rows
```
