# Nest a level in the one before it (same as a later add_level())

For each row in the current data frame, creates N child rows. This is
what
[`add_level`](https://declaredesign.org/r/fabricatr/reference/add_level.md)
does after the first level of a call; `nest_level()` is the spelling
that says so. Parent columns are replicated across children. `N` may be
a scalar (same count for every parent) or a vector of length `nrow` of
the parent (variable children per parent).

## Usage

``` r
nest_level(N, ...)
```

## Arguments

- N:

  Rows per parent. Scalar or per-parent vector.

- ...:

  Column expressions. Note that the `N` visible inside these expressions
  is the total number of rows the level creates, not the per-parent
  count in the `N` argument: nesting 5 citizens in each of 20 villages
  makes `N` equal to 100 here. The behaviour matches fabricatr 1.x, and
  it is what makes `rnorm(N)` draw independently for every village
  rather than drawing five values and reusing them.

## Value

A `fabricatr_level` object, meaningful only as a named argument to
`fabricate`. It replaces each row of the current frame with `N` child
rows, repeating the parent's columns down them, and adds an ID column
named after the argument it is assigned to. That ID is unique across the
whole frame rather than restarting within each parent.

## Examples

``` r
fabricate(
  villages = add_level(N = 20, v_income = rnorm(N)),
  citizens = nest_level(N = 5, income = v_income + rnorm(N))
)
#> # A tibble: 100 × 4
#>    villages v_income citizens  income
#>    <chr>       <dbl> <chr>      <dbl>
#>  1 01         0.0180 001       0.0281
#>  2 01         0.0180 002      -0.659 
#>  3 01         0.0180 003      -0.540 
#>  4 01         0.0180 004      -0.329 
#>  5 01         0.0180 005       1.39  
#>  6 02        -0.872  006      -0.960 
#>  7 02        -0.872  007      -1.98  
#>  8 02        -0.872  008      -1.09  
#>  9 02        -0.872  009      -0.384 
#> 10 02        -0.872  010      -0.838 
#> # ℹ 90 more rows
```
