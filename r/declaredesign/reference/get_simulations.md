# Get the simulations table from a diagnosis

Get the simulations table from a diagnosis

## Usage

``` r
get_simulations(diagnosis)
```

## Arguments

- diagnosis:

  (required) A `diagnosis` object.

## Value

The diagnosis's `simulations_df`: a tibble with one row per estimate per
simulation, as
[`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)
returns it.

## See also

Other simulation and diagnosis:
[`compare_diagnoses()`](https://declaredesign.org/r/declaredesign/reference/compare_diagnoses.md),
[`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md),
[`diagnose_simulations()`](https://declaredesign.org/r/declaredesign/reference/diagnose_simulations.md),
[`get_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/get_diagnosands.md),
[`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)

## Examples

``` r
design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
  declare_inquiry(ATE = 0) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                    label = "ols")
d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
get_simulations(d)
#> # A tibble: 5 × 12
#>   design sim_ID term  estimate std.error statistic p.value conf.low conf.high
#>   <chr>   <int> <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 design      1 Z      -0.308      0.413   -0.747   0.461    -1.15     0.537 
#> 2 design      2 Z      -0.653      0.361   -1.81    0.0810   -1.39     0.0858
#> 3 design      3 Z      -0.256      0.406   -0.630   0.534    -1.09     0.576 
#> 4 design      4 Z       0.143      0.331    0.432   0.669    -0.535    0.822 
#> 5 design      5 Z      -0.0142     0.334   -0.0424  0.966    -0.698    0.670 
#> # ℹ 3 more variables: estimator <chr>, inquiry <chr>, estimand <dbl>
```
