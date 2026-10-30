# Regression tables with estimatr

An `lm_robust` fit carries its own robust standard errors, so a table
package that reads the fit gets the right numbers with no second step.
Two packages have methods for estimatr objects and are what this page
covers: `texreg` and `modelsummary`. Both produce LaTeX, HTML and Word
output from the same call.

``` r

fit1 <- lm_robust(mpg ~ hp, data = mtcars)
fit2 <- lm_robust(mpg ~ hp + wt, data = mtcars)
fit3 <- lm_robust(mpg ~ hp + wt, data = mtcars, clusters = cyl, se_type = "stata")
```

## texreg

estimatr registers `extract` methods for `lm_robust` and `iv_robust`, so
[`texreg()`](https://rdrr.io/pkg/texreg/man/texreg.html) and its
siblings work directly. Confidence intervals are shown by default,
because that is what the fit reports; `include.ci = FALSE` switches to
standard errors and stars.

``` r

library(texreg)
texreg(list(fit1, fit2, fit3), include.ci = FALSE, include.nclusts = FALSE)
#> 
#> \begin{table}
#> \begin{center}
#> \begin{tabular}{l c c c}
#> \hline
#>  & Model 1 & Model 2 & Model 3 \\
#> \hline
#> (Intercept) & $30.10^{***}$ & $37.23^{***}$ & $37.23^{**}$ \\
#>             & $(2.19)$      & $(2.08)$      & $(3.06)$     \\
#> hp          & $-0.07^{***}$ & $-0.03^{***}$ & $-0.03^{*}$  \\
#>             & $(0.01)$      & $(0.01)$      & $(0.01)$     \\
#> wt          &               & $-3.88^{***}$ & $-3.88^{*}$  \\
#>             &               & $(0.69)$      & $(0.70)$     \\
#> \hline
#> R$^2$       & $0.60$        & $0.83$        & $0.83$       \\
#> Adj.\ R$^2$ & $0.59$        & $0.81$        & $0.81$       \\
#> Num.\ obs.  & $32$          & $32$          & $32$         \\
#> RMSE        & $3.86$        & $2.59$        & $2.59$       \\
#> \hline
#> \multicolumn{4}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
#> \end{tabular}
#> \caption{Statistical models}
#> \label{table:coefficients}
#> \end{center}
#> \end{table}
```

`include.nclusts = FALSE` is there because the table mixes clustered and
unclustered fits. texreg fills the `N Clusters` row’s empty cells with
`$$`, which LaTeX will not compile, so when the models mix, drop the row
and say which fits are clustered in the model names, as the next call
does.

[`htmlreg()`](https://rdrr.io/pkg/texreg/man/htmlreg.html) produces HTML
and [`screenreg()`](https://rdrr.io/pkg/texreg/man/screenreg.html)
prints to the console, which is the one to use while you are still
deciding what goes in the table.

``` r

screenreg(list(fit1, fit2, fit3), include.ci = FALSE,
          custom.model.names = c("hp, HC2", "hp + wt, HC2", "hp + wt, CR stata"))
#> 
#> =======================================================
#>              hp, HC2    hp + wt, HC2  hp + wt, CR stata
#> -------------------------------------------------------
#> (Intercept)  30.10 ***  37.23 ***     37.23 **         
#>              (2.19)     (2.08)        (3.06)           
#> hp           -0.07 ***  -0.03 ***     -0.03 *          
#>              (0.01)     (0.01)        (0.01)           
#> wt                      -3.88 ***     -3.88 *          
#>                         (0.69)        (0.70)           
#> -------------------------------------------------------
#> R^2           0.60       0.83          0.83            
#> Adj. R^2      0.59       0.81          0.81            
#> Num. obs.    32         32            32               
#> RMSE          3.86       2.59          2.59            
#> N Clusters                             3               
#> =======================================================
#> *** p < 0.001; ** p < 0.01; * p < 0.05
```

## modelsummary

`modelsummary` reads `lm_robust` fits through `broom`-style methods,
which estimatr provides.

``` r

library(modelsummary)
modelsummary(list("hp, HC2" = fit1, "hp + wt, HC2" = fit2, "hp + wt, CR stata" = fit3),
             output = "markdown", gof_map = c("nobs", "r.squared"))
```

|             | hp, HC2 | hp + wt, HC2 | hp + wt, CR stata |
|-------------|---------|--------------|-------------------|
| (Intercept) | 30.099  | 37.227       | 37.227            |
|             | (2.193) | (2.078)      | (3.061)           |
| hp          | -0.068  | -0.032       | -0.032            |
|             | (0.015) | (0.008)      | (0.005)           |
| wt          |         | -3.878       | -3.878            |
|             |         | (0.688)      | (0.700)           |
| Num.Obs.    | 32      | 32           | 32                |
| R2          | 0.602   | 0.827        | 0.827             |

Because the standard errors already live in the fit, do not pass
`vcov =` to `modelsummary` unless you actually want it to recompute
them. Passing `vcov = "robust"` there overrides estimatr’s HC2 with
`sandwich`’s HC3 default and silently changes the table.

## If you use stargazer

`stargazer` has no method for `lm_robust` objects. estimatr 1.x shipped
[`starprep()`](https://declaredesign.org/r/estimatr/reference/estimatr-defunct.md)
and
[`commarobust()`](https://declaredesign.org/r/estimatr/reference/estimatr-defunct.md)
to bridge that gap, and both are removed in 2.0; they remain as names
that error and say what happened.

``` r

starprep(fit1)
#> Error:
#> ! `starprep()` was removed in estimatr 2.0.
#> It prepared fits for stargazer, which is no longer maintained.
#> Use modelsummary, which reads `tidy()` and `glance()` and so works on every estimator in this package:
#>   modelsummary::modelsummary(list(fit1, fit2))
```

The replacement the error names is `modelsummary`, which is the
recommendation. If a paper is already formatted around stargazer,
extract the pieces and hand them over instead:

``` r

fits <- list(fit1, fit2)
stargazer::stargazer(
  lapply(fits, \(f) lm(f$call$formula, data = mtcars)),
  se = lapply(fits, \(f) f$std.error),
  p  = lapply(fits, \(f) f$p.value)
)
```

The [`lm()`](https://rdrr.io/r/stats/lm.html) refit is only there to
give stargazer an object it recognises; every number that appears in the
table comes from the estimatr fit.
