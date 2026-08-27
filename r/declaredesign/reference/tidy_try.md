# Tidy Model Results and Filter to Relevant Coefficients

Tidy function that returns a tidy data.frame of model results and allows
filtering to relevant coefficients. The function will attempt to tidy
model objects even when they do not have a tidy method available. For
best results, first load the broom package via
[`library(broom)`](https://broom.tidymodels.org/).

## Usage

``` r
tidy_try(fit, term = FALSE)
```

## Arguments

- fit:

  A model fit, as returned by a modeling function like lm, glm, or
  estimatr::lm_robust.

- term:

  A character vector of the terms that represent quantities of interest,
  i.e., "Z". If FALSE, return the first non-intercept term; if TRUE
  return all terms.

## Value

A data.frame with coefficient estimates and associated statistics.

## Examples

``` r

fit <- lm(mpg ~ hp + disp + cyl, data = mtcars)

tidy_try(fit)
#> # A tibble: 4 × 7
#>   term        estimate std.error statistic  p.value conf.low conf.high
#>   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 (Intercept)  34.2       2.59       13.2  1.54e-13  28.9     39.5    
#> 2 hp           -0.0147    0.0147     -1.00 3.25e- 1  -0.0447   0.0153 
#> 3 disp         -0.0188    0.0104     -1.81 8.09e- 2  -0.0401   0.00247
#> 4 cyl          -1.23      0.797      -1.54 1.35e- 1  -2.86     0.406  
```
