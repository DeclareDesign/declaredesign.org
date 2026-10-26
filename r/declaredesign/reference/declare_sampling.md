# Declare a sampling procedure

Draws the sample from the population, keeping the rows where the
sampling indicator `S` is 1. Name the indicator `S`, as in
`declare_sampling(S = complete_rs(N, n = 50))`, or say which rows to
keep with `filter = `; a sampling step that produces neither keeps every
row and warns.

## Usage

``` r
declare_sampling(..., filter = NULL, label = "sampling", draws = 1L)
```

## Arguments

- ...:

  (optional) Named arguments forwarded to
  [`fabricatr::fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.html).
  Unquoted expressions are evaluated lazily in the caller's environment
  with access to the current data frame.

- filter:

  (optional) Unquoted expression evaluated against the data; rows where
  the expression is `TRUE` are retained, and `S` is then not consulted.

- label:

  (optional) Step label. Defaults to `"sampling"`.

- draws:

  (optional) Number of nested draws for this step. Defaults to `1`. When
  `> 1`, the step is re-executed `draws` times for each upstream draw
  during nested simulation.

## Value

A `design_step` (class `c("design_step", "dd", "function")`): a function
that takes the current data frame and returns the rows sampled, with any
declared columns added. Add it to a design with `+`.

## See also

Other design declarations:
[`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md),
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md),
[`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md),
[`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md),
[`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md),
[`declare_notes()`](https://declaredesign.org/r/declaredesign/reference/declare_notes.md),
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md),
[`declare_step()`](https://declaredesign.org/r/declaredesign/reference/declare_step.md),
[`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)

## Examples

``` r
design <-
  declare_model(N = 200, Y = rnorm(N)) +
  declare_sampling(S = sample(rep(0:1, length.out = N)))

nrow(draw_data(design))
#> [1] 100
```
