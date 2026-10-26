# Declare an assignment procedure

Assigns units to treatment conditions, adding the assignment variable
(`Z` by convention) to the data. The randomization itself is usually a
randomizr function such as
[`randomizr::complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.html).

## Usage

``` r
declare_assignment(..., label = "assignment", draws = 1L)
```

## Arguments

- ...:

  (optional) Named arguments forwarded to
  [`fabricatr::fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.html).
  Unquoted expressions are evaluated lazily in the caller's environment
  with access to the current data frame.

- label:

  (optional) Step label. Defaults to `"assignment"`.

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
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md),
[`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md),
[`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md),
[`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md),
[`declare_notes()`](https://declaredesign.org/r/declaredesign/reference/declare_notes.md),
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md),
[`declare_sampling()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md),
[`declare_step()`](https://declaredesign.org/r/declaredesign/reference/declare_step.md),
[`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)

## Examples

``` r
design <-
  declare_model(N = 100, U = rnorm(N)) +
  declare_assignment(Z = sample(rep(0:1, length.out = N)))

table(draw_data(design)$Z)
#> 
#>  0  1 
#> 50 50 
```
