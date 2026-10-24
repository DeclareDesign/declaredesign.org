# Build potential outcomes columns

For each combination of condition values, evaluates the RHS of `x` with
the assignment variable(s) set to those values, producing one column per
condition. The column names follow the pattern
`<outcome>_<var>_<value>`.

## Usage

``` r
potential_outcomes(x, conditions = list(Z = c(0, 1)), sep = "_")
```

## Arguments

- x:

  A two-sided formula. LHS: outcome variable name. RHS: expression for
  the potential outcome (may reference the assignment variable and other
  columns).

- conditions:

  Named list mapping assignment variable name(s) to their possible
  values. Default `list(Z = c(0, 1))`.

- sep:

  Column-name separator. Default `"_"`. Give
  [`reveal_outcomes()`](https://declaredesign.org/r/fabricatr/reference/reveal_outcomes.md)
  the same `sep`.

## Value

A data frame with one column per combination of the values in
`conditions`, named `<outcome><sep><var><sep><value>` and, with more
than one assignment variable, carrying one such pair per variable:
`Y_Z1_0_Z2_0`, `Y_Z1_1_Z2_0`, `Y_Z1_0_Z2_1`, `Y_Z1_1_Z2_1`, the first
variable varying fastest. The names are kept as written even where they
are not syntactic R names, so a negative condition gives `Y_Z_-1` and a
condition `"b c"` gives `Y_Z_b c`; refer to such a column with backticks
or `.data`. Called inside
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
without a name, these columns are appended to the frame.

## Details

When called inside
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
without a left-hand name (i.e. as an unnamed argument), the resulting
columns are appended automatically.

## Examples

``` r
fabricate(
  N = 10, U = rnorm(N),
  potential_outcomes(Y ~ 0.5 * Z + U)
)
#> # A tibble: 10 × 4
#>    ID         U  Y_Z_0  Y_Z_1
#>    <chr>  <dbl>  <dbl>  <dbl>
#>  1 01     0.369  0.369  0.869
#>  2 02    -0.399 -0.399  0.101
#>  3 03    -0.760 -0.760 -0.260
#>  4 04     0.261  0.261  0.761
#>  5 05    -0.731 -0.731 -0.231
#>  6 06     1.64   1.64   2.14 
#>  7 07    -1.11  -1.11  -0.613
#>  8 08     1.22   1.22   1.72 
#>  9 09     0.348  0.348  0.848
#> 10 10    -0.315 -0.315  0.185

# Three conditions
fabricate(
  N = 10, U = rnorm(N),
  potential_outcomes(Y ~ Z * 0.5 + U, conditions = list(Z = 0:2))
)
#> # A tibble: 10 × 5
#>    ID          U   Y_Z_0   Y_Z_1  Y_Z_2
#>    <chr>   <dbl>   <dbl>   <dbl>  <dbl>
#>  1 01     0.692   0.692   1.19    1.69 
#>  2 02     0.333   0.333   0.833   1.33 
#>  3 03    -0.0614 -0.0614  0.439   0.939
#>  4 04    -1.43   -1.43   -0.933  -0.433
#>  5 05     1.01    1.01    1.51    2.01 
#>  6 06    -0.584  -0.584  -0.0841  0.416
#>  7 07     0.752   0.752   1.25    1.75 
#>  8 08     0.523   0.523   1.02    1.52 
#>  9 09    -1.67   -1.67   -1.17   -0.673
#> 10 10     0.654   0.654   1.15    1.65 

# Multi-arm factorial
fabricate(
  N = 10, U = rnorm(N),
  potential_outcomes(Y ~ 0.3 * Z1 + 0.5 * Z2 + U,
                     conditions = list(Z1 = 0:1, Z2 = 0:1))
)
#> # A tibble: 10 × 6
#>    ID         U Y_Z1_0_Z2_0 Y_Z1_1_Z2_0 Y_Z1_0_Z2_1 Y_Z1_1_Z2_1
#>    <chr>  <dbl>       <dbl>       <dbl>       <dbl>       <dbl>
#>  1 01    -0.908      -0.908     -0.608       -0.408     -0.108 
#>  2 02     0.595       0.595      0.895        1.10       1.40  
#>  3 03    -1.04       -1.04      -0.741       -0.541     -0.241 
#>  4 04     1.23        1.23       1.53         1.73       2.03  
#>  5 05     0.339       0.339      0.639        0.839      1.14  
#>  6 06    -0.865      -0.865     -0.565       -0.365     -0.0651
#>  7 07    -1.75       -1.75      -1.45        -1.25      -0.947 
#>  8 08     0.937       0.937      1.24         1.44       1.74  
#>  9 09    -0.288      -0.288      0.0120       0.212      0.512 
#> 10 10    -0.628      -0.628     -0.328       -0.128      0.172 
```
