# Get estimates, inquiries, assignment vectors, or samples from a design given data

Get estimates, inquiries, assignment vectors, or samples from a design
given data

## Usage

``` r
get_estimates(design, data = NULL, start = 1, end = length(design))
```

## Arguments

- design:

  A design object, typically created using the + operator

- data:

  A data.frame object with sufficient information to get the data,
  estimates, inquiries, an assignment vector, or a sample.

- start:

  (Defaults to 1) a scalar indicating which step in the design to begin
  with. By default all data steps are drawn, from step 1 to the last
  step of the design.

- end:

  (Defaults to `length(design)`) a scalar indicating which step in the
  design to finish with.

## Examples

``` r

design <- 
  declare_model(
    N = 100, 
    U = rnorm(N),
    potential_outcomes(Y ~ Z + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N, n = 75)) +
  declare_assignment(Z = complete_ra(N, m = 50)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")

dat <- draw_data(design)

draw_data(design, data = dat, start = 2)
#> # A tibble: 75 × 7
#>    ID          U   Y_Z_0 Y_Z_1     S     Z       Y
#>    <chr>   <dbl>   <dbl> <dbl> <dbl> <int>   <dbl>
#>  1 001   -0.884  -0.884  0.116     1     1  0.116 
#>  2 002    1.49    1.49   2.49      1     1  2.49  
#>  3 003    0.835   0.835  1.83      1     1  1.83  
#>  4 004    0.399   0.399  1.40      1     0  0.399 
#>  5 006   -0.0461 -0.0461 0.954     1     0 -0.0461
#>  6 008    0.488   0.488  1.49      1     1  1.49  
#>  7 013   -0.117  -0.117  0.883     1     1  0.883 
#>  8 014    0.891   0.891  1.89      1     0  0.891 
#>  9 015   -0.769  -0.769  0.231     1     1  0.231 
#> 10 016    0.905   0.905  1.91      1     1  1.91  
#> # ℹ 65 more rows

get_estimates(design, data = dat)
#>   estimator term  estimate std.error statistic    p.value  conf.low conf.high
#> 1 estimator    Z 0.5545079 0.2133693  2.598818 0.01131166 0.1292635 0.9797523
#>   df outcome inquiry
#> 1 73       Y     ATE
```
