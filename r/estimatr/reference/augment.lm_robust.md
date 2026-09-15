# Augment a Model Object with Fitted Values and Residuals

Returns the model frame with `.fitted` and `.resid` columns appended,
the form downstream packages expect from
[`broom::augment()`](https://generics.r-lib.org/reference/augment.html).
Supplying `newdata` returns that instead, with `.fitted` only.

## Usage

``` r
# S3 method for class 'lm_robust'
augment(x, data = NULL, newdata = NULL, ...)

# S3 method for class 'iv_robust'
augment(x, data = NULL, newdata = NULL, ...)
```

## Arguments

- x:

  An `lm_robust` or `iv_robust` object.

- data:

  The data to augment, defaulting to the model frame.

- newdata:

  Optional new data to predict on instead.

- ...:

  (optional) Ignored.

## Value

A `data.frame`.

## Examples

``` r
set.seed(55)
dat <- data.frame(x = rnorm(50), z = rep(0:1, 25))
dat$y <- dat$x + 0.4 * dat$z + rnorm(50)
fit <- lm_robust(y ~ x + z, data = dat)

head(augment(fit))
#> # A tibble: 6 × 5
#>        y        x     z .fitted  .resid
#>    <dbl>    <dbl> <int>   <dbl>   <dbl>
#> 1 -1.04   0.120       0 -0.0837 -0.960 
#> 2 -1.09  -1.81        1 -1.06   -0.0211
#> 3 -0.877  0.152       0 -0.0587 -0.819 
#> 4 -1.40  -1.12        1 -0.514  -0.888 
#> 5 -0.158  0.00191     0 -0.178   0.0194
#> 6  1.81   1.19        1  1.32    0.492 

# Supplying newdata returns predictions on it, with .fitted only
head(augment(fit, newdata = dat[1:5, ]))
#> # A tibble: 5 × 4
#>          x     z      y .fitted
#>      <dbl> <int>  <dbl>   <dbl>
#> 1  0.120       0 -1.04  -0.0837
#> 2 -1.81        1 -1.09  -1.06  
#> 3  0.152       0 -0.877 -0.0587
#> 4 -1.12        1 -1.40  -0.514 
#> 5  0.00191     0 -0.158 -0.178 
```
