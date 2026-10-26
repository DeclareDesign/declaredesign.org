# Summarize a design

Runs the design once and reports what each step did: the rows a data
step produced or kept and the columns it added, dropped, or changed; the
value each inquiry took; the estimate each estimator returned. The run
itself, estimates joined to estimands, comes after the steps, then the
parameters and objects the design refers to. Everything the printout
shows is on the returned object, so a document or an app can use the
pieces.

## Usage

``` r
# S3 method for class 'design'
summary(object, run = TRUE, ...)
```

## Arguments

- object:

  (required) A `design`.

- run:

  (optional) Whether to run the design once. Defaults to `TRUE`.

- ...:

  (optional) Ignored.

## Value

A `summary.design`: a list with `steps` (a tibble with one row per step:
`step`, `label`, `type`, `call`, and `one_run`, the account of that step
on this run), `parameters` and `notes` (as
[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md)
and
[`design_notes()`](https://declaredesign.org/r/declaredesign/reference/design_notes.md)
return them), `ran` (logical), and when the design was run, `data` (the
final data), `inquiries` and `estimates` (as
[`run_design()`](https://declaredesign.org/r/declaredesign/reference/run_design.md)
returns them).

## Details

The run is one random draw, so the numbers differ from call to call. A
design whose estimator is slow can be summarized without running it:
`run = FALSE` gives the steps and the parameters alone, which is what
[`print()`](https://rdrr.io/r/base/print.html) shows.

## Examples

``` r
n_units <- 30
design <- declare_model(N = n_units, U = rnorm(N), Y = U) +
  declare_inquiry(mu = mean(Y)) +
  declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)",
                    inquiry = "mu", label = "ols")
summary(design)
#> Research design with 3 steps
#> 
#> Step 1 (model): declare_model(N = n_units, U = rnorm(N), Y = U)
#>   N = 30 rows; adds ID, U, Y
#> Step 2 (inquiry): declare_inquiry(mu = mean(Y))
#>   mu = 0.0466
#> Step 3 (estimator): declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)", inquiry = "mu", label = "ols")
#>   (Intercept) = 0.0466 (std.error 0.185)
#> 
#> One run of the design:
#> # A tibble: 1 × 10
#>   term        estimate std.error statistic p.value conf.low conf.high estimator
#>   <chr>          <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <chr>    
#> 1 (Intercept)   0.0466     0.185     0.252   0.803   -0.332     0.425 ols      
#> # ℹ 2 more variables: inquiry <chr>, estimand <dbl>
#> 
#> Parameters and objects the design refers to:
#>     name value   kind declared steps
#>  n_units    30 scalar    FALSE     1
summary(design, run = FALSE)
#> Research design with 3 steps
#> 
#> Step 1 (model): declare_model(N = n_units, U = rnorm(N), Y = U)
#> Step 2 (inquiry): declare_inquiry(mu = mean(Y))
#> Step 3 (estimator): declare_estimator(Y ~ 1, .method = lm, term = "(Intercept)", inquiry = "mu", label = "ols")
#> 
#> Parameters and objects the design refers to:
#>     name value   kind declared steps
#>  n_units    30 scalar    FALSE     1
summary(design)$steps
#> # A tibble: 3 × 5
#>    step label type      call                                             one_run
#>   <int> <chr> <chr>     <chr>                                            <chr>  
#> 1     1 model model     "declare_model(N = n_units, U = rnorm(N), Y = U… N = 30…
#> 2     2 mu    inquiry   "declare_inquiry(mu = mean(Y))"                  mu = 0…
#> 3     3 ols   estimator "declare_estimator(Y ~ 1, .method = lm, term = … (Inter…
```
