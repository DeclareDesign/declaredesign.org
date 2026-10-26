# Declare the data-generating model

The model describes the world the design runs in. It does not have to be
correct: declaring one you doubt is how you find out whether the design
still works when you are wrong about it.

## Usage

``` r
declare_model(..., label = "model", draws = 1L)
```

## Arguments

- ...:

  (optional) Named arguments forwarded to
  [`fabricatr::fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.html).
  Unquoted expressions are evaluated lazily in the caller's environment
  with access to the current data frame.

- label:

  (optional) Step label. Defaults to `"model"`.

- draws:

  (optional) Number of nested draws for this step. Defaults to `1`. When
  `> 1`, the step is re-executed `draws` times for each upstream draw
  during nested simulation.

## Value

A `design_step` (class `c("design_step", "dd", "function")`): a function
that takes the current data frame and returns a tibble with the declared
columns added (or built from scratch, for the first step). Add it to a
design with `+`.

## Details

Declares a step that builds (or augments) the population data via
[`fabricatr::fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.html).
The first model step in a design receives `data = NULL`, so it must
specify `N` (and any variables) directly. Subsequent model steps add
columns to the existing data.

## See also

Other design declarations:
[`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md),
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md),
[`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md),
[`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md),
[`declare_notes()`](https://declaredesign.org/r/declaredesign/reference/declare_notes.md),
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md),
[`declare_sampling()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md),
[`declare_step()`](https://declaredesign.org/r/declaredesign/reference/declare_step.md),
[`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)

## Examples

``` r
design <- declare_model(N = 50, U = rnorm(N), Y = U + 1)

head(draw_data(design))
#> # A tibble: 6 × 3
#>   ID         U     Y
#>   <chr>  <dbl> <dbl>
#> 1 01    -0.527 0.473
#> 2 02    -0.122 0.878
#> 3 03    -0.586 0.414
#> 4 04    -0.248 0.752
#> 5 05     0.801 1.80 
#> 6 06     2.53  3.53 

# A second model step adds columns to the data the first one made.
design <-
  declare_model(N = 50, U = rnorm(N)) +
  declare_model(Y_Z_0 = U, Y_Z_1 = U + 0.5)

head(draw_data(design))
#> # A tibble: 6 × 4
#>   ID          U   Y_Z_0  Y_Z_1
#>   <chr>   <dbl>   <dbl>  <dbl>
#> 1 01    -0.149  -0.149   0.351
#> 2 02     0.322   0.322   0.822
#> 3 03    -0.324  -0.324   0.176
#> 4 04    -0.783  -0.783  -0.283
#> 5 05    -0.0181 -0.0181  0.482
#> 6 06    -0.169  -0.169   0.331
```
