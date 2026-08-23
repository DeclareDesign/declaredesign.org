# Build lm_robust object from lm fit

Build lm_robust object from lm fit

## Usage

``` r
commarobust(model, se_type = NULL, clusters = NULL, ci = TRUE, alpha = 0.05)
```

## Arguments

- model:

  an lm model object

- se_type:

  The sort of standard error sought. If `clusters` is not specified the
  options are "HC0", "HC1" (or "stata", the equivalent), "HC2"
  (default), "HC3", or "classical". If `clusters` is specified the
  options are "CR0", "CR2" (default), or "stata". Can also specify
  "none", which may speed up estimation of the coefficients.

- clusters:

  A vector corresponding to the clusters in the data.

- ci:

  logical. Whether to compute and return p-values and confidence
  intervals, TRUE by default.

- alpha:

  The significance level, 0.05 by default.

## Value

an
[`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
object.

## Examples

``` r
lmo <- lm(mpg ~ hp, data = mtcars)

# Default HC2
commarobust(lmo)
#>                Estimate Std. Error   t value    Pr(>|t|)    CI Lower
#> (Intercept) 30.09886054 2.19301194 13.724896 1.81366e-14 25.62013267
#> hp          -0.06822828 0.01471473 -4.636732 6.48546e-05 -0.09827977
#>                CI Upper DF
#> (Intercept) 34.57758841 30
#> hp          -0.03817678 30

commarobust(lmo, se_type = "HC3")
#>                Estimate Std. Error   t value     Pr(>|t|)   CI Lower
#> (Intercept) 30.09886054 2.41006671 12.488808 2.044330e-13 25.1768477
#> hp          -0.06822828 0.01660193 -4.109659 2.822529e-04 -0.1021339
#>                CI Upper DF
#> (Intercept) 35.02087341 30
#> hp          -0.03432261 30

commarobust(lmo, se_type = "stata", clusters = mtcars$carb)
#>                Estimate Std. Error   t value     Pr(>|t|)   CI Lower
#> (Intercept) 30.09886054 2.15609050 13.959924 3.390807e-05 24.5564535
#> hp          -0.06822828 0.01404901 -4.856449 4.647551e-03 -0.1043424
#>                CI Upper DF
#> (Intercept) 35.64126761  5
#> hp          -0.03211416  5
```
