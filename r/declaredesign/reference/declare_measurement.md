# Declare measurement

Records what the study observes: the measured outcome, typically `Y`
revealed from the potential outcomes given the assignment `Z`. Columns
are added to the data the way
[`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md)
adds them, without a new ID column.

## Usage

``` r
declare_measurement(..., label = "measurement", draws = 1L)
```

## Arguments

- ...:

  (optional) Named arguments forwarded to
  [`fabricatr::fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.html).
  Unquoted expressions are evaluated lazily in the caller's environment
  with access to the current data frame.

- label:

  (optional) Step label. Defaults to `"measurement"`.

- draws:

  (optional) Number of nested draws for this step. Defaults to `1`. When
  `> 1`, the step is re-executed `draws` times for each upstream draw
  during nested simulation.

## Value

A `design_step` (class `c("design_step", "dd", "function")`): a function
that takes the current data frame and returns it with the declared
columns added. Add it to a design with `+`.

## See also

Other design declarations:
[`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md),
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md),
[`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md),
[`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md),
[`declare_notes()`](https://declaredesign.org/r/declaredesign/reference/declare_notes.md),
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md),
[`declare_sampling()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md),
[`declare_step()`](https://declaredesign.org/r/declaredesign/reference/declare_step.md),
[`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)

## Examples

``` r
design <-
  declare_model(N = 100, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
  declare_assignment(Z = sample(rep(0:1, length.out = N))) +
  declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z)

head(draw_data(design))
#> # A tibble: 6 × 6
#>   ID         U  Y_Z_0  Y_Z_1     Z      Y
#>   <chr>  <dbl>  <dbl>  <dbl> <int>  <dbl>
#> 1 001   -1.06  -1.06  -0.563     0 -1.06 
#> 2 002    1.63   1.63   2.13      0  1.63 
#> 3 003    0.541  0.541  1.04      1  1.04 
#> 4 004    1.46   1.46   1.96      0  1.46 
#> 5 005   -1.93  -1.93  -1.43      0 -1.93 
#> 6 006   -0.880 -0.880 -0.380     0 -0.880
```
