# Resample a data frame, with optional hierarchical cluster resampling

For simple bootstrap resampling, call `resample_data(data)` with no
other arguments. For cluster resampling, supply `N` and `ID_labels`. For
multi-level hierarchical resampling, supply vectors for both.

## Usage

``` r
resample_data(data, N, ID_labels = NULL, unique_labels = FALSE)
```

## Arguments

- data:

  A data frame.

- N:

  Resample size. May be:

  - Missing: resample `nrow(data)` units (standard bootstrap).

  - A single unnamed number, with no `ID_labels`: resample that many
    rows with replacement.

  - A named integer vector: names are cluster ID column names (from
    outermost to innermost), values are the number of units to resample
    at each level. Use the constant `ALL` to pass through all units at a
    level.

  - An unnamed integer vector: same as above but supply `ID_labels`
    separately.

- ID_labels:

  Character vector of cluster ID column names (outermost to innermost).
  Required when `N` is unnamed.

- unique_labels:

  Logical. When `TRUE`, adds a `<ID>_unique` column with globally unique
  labels after resampling (useful for multi-level bootstraps where the
  same cluster is drawn multiple times). Default `FALSE`.

## Value

A tibble of the resampled rows: `nrow(data)` of them for the default
bootstrap, `N` for an unnamed scalar `N`, and for a hierarchical call
the sizes requested at each level multiplied together. ID columns are
carried over from the rows that were drawn rather than regenerated, so a
cluster drawn twice appears twice under one ID; pass
`unique_labels = TRUE` to add an `<ID>_unique` column that tells the
copies apart.

## Examples

``` r
# Simple bootstrap
df <- fabricate(N = 50, Y = rnorm(N))
boot <- resample_data(df)
nrow(boot)
#> [1] 50

# Cluster bootstrap: resample 10 of 20 clusters
clustered <- fabricate(
  clusters = add_level(N = 20, gdp = rnorm(N)),
  units    = nest_level(N = 5, Y = gdp + rnorm(N))
)
resample_data(clustered, N = c(clusters = 10))
#> # A tibble: 50 × 4
#>    clusters    gdp units       Y
#>    <chr>     <dbl> <chr>   <dbl>
#>  1 06       -0.228 026   -0.910 
#>  2 06       -0.228 027    1.19  
#>  3 06       -0.228 028   -1.08  
#>  4 06       -0.228 029   -0.0445
#>  5 06       -0.228 030    1.37  
#>  6 09        0.923 041    0.826 
#>  7 09        0.923 042    1.50  
#>  8 09        0.923 043   -0.730 
#>  9 09        0.923 044    0.110 
#> 10 09        0.923 045    1.39  
#> # ℹ 40 more rows
```
