# Keep every unit at a resampling level

Use `ALL` in the `N` argument of `resample_data` to keep every unit at a
given hierarchical level while still resampling inner levels. Leaving a
level out of `N` does the same thing: `N = c(clusters = 2)` and
`N = c(clusters = 2, units = ALL)` return the same frame.

## Usage

``` r
ALL
```

## Format

A single integer used as a sentinel. Its value carries no meaning and
should not be relied on or arithmetic done with it; write `ALL` itself
wherever a level is to pass through whole.

## Examples

``` r
clustered <- fabricate(
  clusters = add_level(N = 4, gdp = rnorm(N)),
  units    = nest_level(N = 2, Y = gdp + rnorm(N))
)
# Resample 2 clusters, keeping every unit inside the ones drawn
resample_data(clustered, N = c(clusters = 2, units = ALL))
#> # A tibble: 4 × 4
#>   clusters   gdp units      Y
#>   <chr>    <dbl> <chr>  <dbl>
#> 1 1        -1.40 1     -0.778
#> 2 1        -1.40 2     -0.252
#> 3 1        -1.40 1     -0.778
#> 4 1        -1.40 2     -0.252
```
