# Print a diagnosand comparison

Print a diagnosand comparison

## Usage

``` r
# S3 method for class 'compared_diagnoses'
print(x, ...)
```

## Arguments

- x:

  (required) A `compared_diagnoses` object, as returned by
  [`compare_diagnoses()`](https://declaredesign.org/r/declaredesign/reference/compare_diagnoses.md).

- ...:

  (optional) Passed to the
  [`print()`](https://rdrr.io/r/base/print.html) method of
  `compared_diagnoses_df`.

## Value

`x`, invisibly.

## Examples

``` r
design <- declare_parameters(n_units = 100) +
  declare_model(N = n_units, Y = rnorm(N), Z = randomizr::complete_ra(N)) +
  declare_inquiry(ATE = 0) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                    label = "ols")
print(compare_diagnoses(design, redesign(design, n_units = 200),
                        sims = 20, bootstrap_sims = 20))
#> Comparison of diagnosands, design 2 minus design 1
#> 
#> # A tibble: 7 × 13
#>   inquiry estimator term  n_units_2 diagnosand    mean_1  mean_2 mean_difference
#>   <chr>   <chr>     <chr>     <dbl> <chr>          <dbl>   <dbl>           <dbl>
#> 1 ATE     ols       Z           200 mean_estima… 0        0               0     
#> 2 ATE     ols       Z           200 mean_estima… 0.00698 -0.0109         -0.0179
#> 3 ATE     ols       Z           200 bias         0.00698 -0.0109         -0.0179
#> 4 ATE     ols       Z           200 sd_estimate  0.218    0.110          -0.109 
#> 5 ATE     ols       Z           200 rmse         0.213    0.107          -0.105 
#> 6 ATE     ols       Z           200 power        0.1      0              -0.1   
#> 7 ATE     ols       Z           200 coverage     0.9      1               0.1   
#> # ℹ 5 more variables: se_1 <dbl>, se_2 <dbl>, se_difference <dbl>,
#> #   conf.low <dbl>, conf.high <dbl>
```
