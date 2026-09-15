# Functions removed in 2.0

`commarobust()` and `starprep()` were helpers for producing robust
standard errors outside the package's own estimators and formatting them
for stargazer. Both are removed.

## Usage

``` r
commarobust(...)

starprep(...)
```

## Arguments

- ...:

  (optional) Ignored.

## Value

Never returns; both functions signal an error.

## Details

They are kept here as names that error rather than deleted outright, so
that a script written against estimatr 1.x says what happened and what
to do instead of failing with `could not find function`.

`commarobust()` recomputed robust standard errors on a fitted `lm`. Fit
the model with
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
instead, which is what it was reimplementing.

`starprep()` prepared a list of fits for stargazer, which has not been
maintained for years. Table-building now goes through modelsummary,
which reads [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
and [`glance()`](https://generics.r-lib.org/reference/glance.html) and
therefore works on every estimator in this package without any adapter.

## Examples

``` r
# Both of these error. The replacements:
set.seed(1)
dat <- data.frame(y = rnorm(20), z = rep(0:1, 10))

# was: commarobust(lm(y ~ z, data = dat))
lm_robust(y ~ z, data = dat)
#>               Estimate Std. Error    t value  Pr(>|t|)   CI Lower  CI Upper DF
#> (Intercept)  0.2751138  0.2499575  1.1006423 0.2855541 -0.2500274 0.8002551 18
#> z           -0.1691799  0.4177121 -0.4050155 0.6902390 -1.0467604 0.7084007 18

# was: starprep(fit1, fit2) |> stargazer::stargazer()
# now: modelsummary::modelsummary(list(fit1, fit2))
```
