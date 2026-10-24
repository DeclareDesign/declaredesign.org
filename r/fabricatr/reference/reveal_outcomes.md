# Reveal observed outcomes from potential outcomes columns

Implements the switching equation: for each unit, selects the potential
outcome column corresponding to that unit's realized assignment.

## Usage

``` r
reveal_outcomes(x, sep = "_")
```

## Arguments

- x:

  A formula of the form `outcome ~ assignment` or `outcome ~ Z1 + Z2`
  for factorial assignments. The outcome name and assignment variable(s)
  must match columns already in the data (typically created by
  `potential_outcomes`).

- sep:

  Column-name separator. Default `"_"`. It must match the `sep` given to
  [`potential_outcomes()`](https://declaredesign.org/r/fabricatr/reference/potential_outcomes.md).

## Value

A vector with one element per row, holding each unit's outcome under the
assignment it actually received, and carrying the type of the potential
outcome columns it reads. A factor reveals as a factor, keeping its
levels and their order, and a `Date` as a `Date`. Where the conditions
do not agree on one set of factor levels there are none to keep, so the
result is `character` and a warning says so.

## Examples

``` r
dat <- fabricate(
  N = 10, U = rnorm(N),
  potential_outcomes(Y ~ 0.5 * Z + U)
)
fabricate(
  data = dat,
  Z = rbinom(N, 1, 0.5),
  Y = reveal_outcomes(Y ~ Z)
)
#> # A tibble: 10 × 6
#>    ID          U   Y_Z_0  Y_Z_1     Z       Y
#>    <chr>   <dbl>   <dbl>  <dbl> <int>   <dbl>
#>  1 01     0.143   0.143   0.643     1  0.643 
#>  2 02     1.02    1.02    1.52      0  1.02  
#>  3 03    -2.40   -2.40   -1.90      1 -1.90  
#>  4 04     0.218   0.218   0.718     0  0.218 
#>  5 05    -0.396  -0.396   0.104     0 -0.396 
#>  6 06     3.13    3.13    3.63      1  3.63  
#>  7 07    -0.0268 -0.0268  0.473     0 -0.0268
#>  8 08    -0.753  -0.753  -0.253     1 -0.253 
#>  9 09     0.0182  0.0182  0.518     1  0.518 
#> 10 10    -0.742  -0.742  -0.242     0 -0.742 

# A separator other than "_" is given to both
fabricate(
  N = 10, U = rnorm(N),
  potential_outcomes(Y ~ 0.5 * Z + U, sep = "."),
  Z = rbinom(N, 1, 0.5),
  Y = reveal_outcomes(Y ~ Z, sep = ".")
)
#> # A tibble: 10 × 6
#>    ID         U  Y.Z.0  Y.Z.1     Z      Y
#>    <chr>  <dbl>  <dbl>  <dbl> <int>  <dbl>
#>  1 01    -0.220 -0.220  0.280     0 -0.220
#>  2 02    -0.774 -0.774 -0.274     0 -0.774
#>  3 03     2.32   2.32   2.82      0  2.32 
#>  4 04     1.53   1.53   2.03      1  2.03 
#>  5 05     0.635  0.635  1.13      1  1.13 
#>  6 06    -0.149 -0.149  0.351     0 -0.149
#>  7 07    -0.240 -0.240  0.260     0 -0.240
#>  8 08    -1.51  -1.51  -1.01      0 -1.51 
#>  9 09     1.02   1.02   1.52      1  1.52 
#> 10 10    -1.04  -1.04  -0.541     0 -1.04 
```
