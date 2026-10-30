# Tidy an estimatr object

Tidy an estimatr object

## Usage

``` r
# S3 method for class 'lm_robust'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)

# S3 method for class 'iv_robust'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)

# S3 method for class 'difference_in_means'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)

# S3 method for class 'horvitz_thompson'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)

# S3 method for class 'lh_robust'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)

# S3 method for class 'lh'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)
```

## Arguments

- x:

  An object returned by one of the estimators

- conf.int:

  Logical, whether to include confidence intervals.

- conf.level:

  The confidence level for intervals.

- ...:

  (optional) Ignored.

## Value

A tibble with one row per term (and per outcome, for a multivariate
fit): `term`, `estimate`, `std.error`, `statistic`, `p.value`,
`conf.low`, `conf.high`, `df`, and `outcome`. A tibble rather than a
plain data frame, as broom's tidiers return; 1.x returned a data frame.

## Examples

``` r
set.seed(50)
dat <- data.frame(x = rnorm(50), z = rep(0:1, 25))
dat$y <- dat$x + 0.4 * dat$z + rnorm(50)
fit <- lm_robust(y ~ x + z, data = dat)

# One row per term, with the interval the fit was built with
tidy(fit)
#> # A tibble: 3 × 9
#>   term     estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>       <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Interc…   -0.354     0.187     -1.89 6.43e-2   -0.731    0.0219    47 y      
#> 2 x           0.753     0.135      5.60 1.09e-6    0.483    1.02      47 y      
#> 3 z           0.851     0.279      3.05 3.74e-3    0.290    1.41      47 y      
tidy(fit, conf.int = FALSE)
#> # A tibble: 3 × 7
#>   term        estimate std.error statistic    p.value    df outcome
#>   <chr>          <dbl>     <dbl>     <dbl>      <dbl> <dbl> <chr>  
#> 1 (Intercept)   -0.354     0.187     -1.89 0.0643        47 y      
#> 2 x              0.753     0.135      5.60 0.00000109    47 y      
#> 3 z              0.851     0.279      3.05 0.00374       47 y      
tidy(fit, conf.level = 0.9)
#> # A tibble: 3 × 9
#>   term     estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>       <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 (Interc…   -0.354     0.187     -1.89 6.43e-2   -0.668   -0.0405    47 y      
#> 2 x           0.753     0.135      5.60 1.09e-6    0.528    0.979     47 y      
#> 3 z           0.851     0.279      3.05 3.74e-3    0.383    1.32      47 y      

# The same shape for every estimator in the package
tidy(difference_in_means(y ~ z, data = dat))
#> # A tibble: 1 × 9
#>   term  estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 z        0.875     0.347      2.52  0.0151    0.177      1.57  46.8 y      
```
