# Simulate one or more designs

Runs each design `sims` times, collecting estimands and estimates into a
single long tibble suitable for diagnosis. A `design` column names the
design each row came from, whether one design is supplied or several.

## Usage

``` r
simulate_design(..., sims = NULL, progress = FALSE)

simulate_designs(..., sims = NULL, progress = FALSE)
```

## Arguments

- ...:

  (required) One or more `design` objects, or lists of them.

- sims:

  (optional) Number of simulations per design. Defaults to `NULL`, which
  means 500 flat simulations for a design with no step-level `draws`. A
  design with step-level `draws` runs in nested mode whether or not
  `sims` is supplied; supplying it warns and is otherwise ignored.

- progress:

  (optional) If `TRUE`, display a progress bar for this call by wrapping
  it in
  [`progressr::with_progress()`](https://progressr.futureverse.org/reference/with_progress.html).
  The better habit is to opt in once per session with
  `progressr::handlers(global = TRUE)`, which covers every call and lets
  you choose how progress is shown. Nothing is displayed by default.

## Value

A tibble with one row per estimate per simulation: `design`, `sim_ID`,
the estimate columns (`term`, `estimate`, `std.error`, `p.value`,
`conf.low`, `conf.high`, and so on), `estimator`, `inquiry`, `estimand`,
and a column for each redesign parameter. A nested simulation adds a
`<label>_draw` column for each step with `draws > 1`.

## Details

If any step in a design has `draws > 1`, the simulation runs in nested
mode: each step with `draws > 1` fans out, and the total simulation
count equals the product of all `draws` values across steps. Declared
`draws` take priority over `sims`, so supplying both runs the nested
simulation and warns, printing the plan it is running instead.

Steps upstream of the first fan-out run once and are held fixed across
the draws below them, which is what makes
`declare_assignment(..., draws = 50)` mean fifty randomizations of one
population.

Parallelism is handled through the `future` ecosystem. Call
`future::plan(multisession, workers = 4)` before `simulate_design()`
and, if the `furrr` package is installed, simulations run in parallel
with no other change. Every draw runs on its own L'Ecuyer-CMRG
random-number stream whichever plan is active, so
[`set.seed()`](https://rdrr.io/r/base/Random.html) before the call gives
the same table sequentially and in parallel. The streams are not those
of DeclareDesign 1.x, so a 1.x seed does not reproduce a 1.x table here.

## See also

Other simulation and diagnosis:
[`compare_diagnoses()`](https://declaredesign.org/r/declaredesign/reference/compare_diagnoses.md),
[`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md),
[`diagnose_simulations()`](https://declaredesign.org/r/declaredesign/reference/diagnose_simulations.md),
[`get_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/get_diagnosands.md),
[`get_simulations()`](https://declaredesign.org/r/declaredesign/reference/get_simulations.md)

## Examples

``` r
design <- declare_model(N = 30, U = rnorm(N), Z = rep(0:1, 15), Y = U + Z) +
  declare_inquiry(ATE = 1) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                    label = "ols")
simulate_design(design, sims = 3)
#> # A tibble: 3 × 12
#>   design sim_ID term  estimate std.error statistic  p.value conf.low conf.high
#>   <chr>   <int> <chr>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 design      1 Z        1.44      0.344     4.19  0.000252    0.737      2.15
#> 2 design      2 Z        0.738     0.431     1.71  0.0982     -0.146      1.62
#> 3 design      3 Z        0.308     0.427     0.721 0.477      -0.566      1.18
#> # ℹ 3 more variables: estimator <chr>, inquiry <chr>, estimand <dbl>
```
