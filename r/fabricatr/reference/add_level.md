# Add a level, nested in the one before it if there is one

The first `add_level()` in a
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
call creates `N` rows, since there is nothing above it to nest in. A
later one nests: it creates `N` rows inside each row of the level before
it, exactly as
[`nest_level`](https://declaredesign.org/r/fabricatr/reference/nest_level.md)
does, and the two can be used interchangeably there. For independent
(non-nested) levels used in `cross_levels` or `link_levels`, use
`declare_level`.

## Usage

``` r
add_level(N, ...)
```

## Arguments

- N:

  Number of rows to create.

- ...:

  Column expressions evaluated sequentially. `N` is available as a
  scalar integer.

## Value

A `fabricatr_level` object, meaningful only as a named argument to
`fabricate`. It contributes `N` rows, an ID column named after the
argument it is assigned to, and one column per expression in `...`. Any
level nested below it repeats these rows.

## Examples

``` r
fabricate(
  villages = add_level(N = 20, income = rnorm(N)),
  citizens = nest_level(N = 5, Y = income + rnorm(N))
)
#> # A tibble: 100 × 4
#>    villages income citizens        Y
#>    <chr>     <dbl> <chr>       <dbl>
#>  1 01       -1.63  001      -0.00744
#>  2 01       -1.63  002      -1.52   
#>  3 01       -1.63  003      -1.76   
#>  4 01       -1.63  004      -3.54   
#>  5 01       -1.63  005      -1.91   
#>  6 02        0.512 006       0.199  
#>  7 02        0.512 007       1.58   
#>  8 02        0.512 008       0.582  
#>  9 02        0.512 009      -0.127  
#> 10 02        0.512 010       0.462  
#> # ℹ 90 more rows
```
