# Declare an independent level for cross-classification

Creates N rows as a standalone data frame registered for use by
`cross_levels` or `link_levels`. Unlike `add_level`, a `declare_level`
call does not nest into any existing hierarchy: it starts fresh. Use
this when you need two independent populations to cross (e.g., countries
and years for panel data).

## Usage

``` r
declare_level(N, ...)
```

## Arguments

- N:

  Number of rows to create.

- ...:

  Column expressions evaluated sequentially. `N` is available as a
  scalar integer.

## Value

A `fabricatr_level` object, meaningful only as a named argument to
`fabricate`. It builds `N` rows and an ID column named after the
argument it is assigned to, and holds them aside: the rows reach the
fabricated frame only when a later `cross_levels` or `link_levels` names
this level in `.by`. Declaring a level that nothing crosses or links
contributes nothing to the result.

## Examples

``` r
fabricate(
  countries = declare_level(N = 20, gdp = runif(N, 1, 10)),
  years     = declare_level(N = 10, shock = runif(N, 1, 5)),
  obs       = cross_levels(
    .by = c("countries", "years"),
    GDP_it = gdp + shock
  )
)
#> # A tibble: 200 × 6
#>    countries   gdp years shock obs   GDP_it
#>    <chr>     <dbl> <chr> <dbl> <chr>  <dbl>
#>  1 01         4.90 01     1.45 001     6.34
#>  2 02         7.85 01     1.45 002     9.30
#>  3 03         3.64 01     1.45 003     5.09
#>  4 04         1.97 01     1.45 004     3.41
#>  5 05         1.66 01     1.45 005     3.10
#>  6 06         4.21 01     1.45 006     5.65
#>  7 07         6.14 01     1.45 007     7.59
#>  8 08         7.10 01     1.45 008     8.55
#>  9 09         7.22 01     1.45 009     8.66
#> 10 10         4.36 01     1.45 010     5.81
#> # ℹ 190 more rows
```
