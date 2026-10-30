# Extract model data for the texreg package

Prepares an `lm_robust` or `iv_robust` fit for texreg. Largely a clone
of texreg's own `extract.lm` method.

## Usage

``` r
extract.lm_robust(
  model,
  include.ci = TRUE,
  include.rsquared = TRUE,
  include.adjrs = TRUE,
  include.nobs = TRUE,
  include.fstatistic = FALSE,
  include.rmse = TRUE,
  include.nclusts = TRUE,
  ...
)

extract.iv_robust(
  model,
  include.ci = TRUE,
  include.rsquared = TRUE,
  include.adjrs = TRUE,
  include.nobs = TRUE,
  include.fstatistic = FALSE,
  include.rmse = TRUE,
  include.nclusts = TRUE,
  ...
)
```

## Arguments

- model:

  An `lm_robust` or `iv_robust` fit.

- include.ci, include.rsquared, include.adjrs, include.nobs:

  Logical.

- include.fstatistic, include.rmse, include.nclusts:

  Logical.

- ...:

  (optional) Ignored.

## Value

A texreg object.

## Details

These are exported as plain functions rather than registered with
`S3method()` because that is how texreg finds them: it looks up
`extract.<class>` by name in the package namespace rather than
dispatching on a generic it owns. Registering them the usual way would
leave texreg unable to see them.

texreg is the only consumer. Table building through modelsummary needs
nothing here, since it reads
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`glance()`](https://generics.r-lib.org/reference/glance.html) and so
already works on every estimator in this package.

## Examples

``` r
set.seed(60)
dat <- data.frame(x = rnorm(50), z = rep(0:1, 25))
dat$y <- dat$x + 0.4 * dat$z + rnorm(50)
fit <- lm_robust(y ~ x + z, data = dat)

if (requireNamespace("texreg", quietly = TRUE)) {
  texreg::screenreg(fit)
}
#> 
#> ==========================
#>              Model 1      
#> --------------------------
#> (Intercept)    0.32       
#>              [-0.03; 0.67]
#> x              0.98 *     
#>              [ 0.68; 1.28]
#> z              0.04       
#>              [-0.49; 0.56]
#> --------------------------
#> R^2            0.55       
#> Adj. R^2       0.53       
#> Num. obs.     50          
#> RMSE           0.92       
#> ==========================
#> * 0 outside the confidence interval.
```
