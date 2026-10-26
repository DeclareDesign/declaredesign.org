# Declare an inquiry

Each named argument records an inquiry: an expression evaluated on the
data as it stands when the step runs. Its value on a given draw is the
estimand.

## Usage

``` r
declare_inquiry(
  ...,
  subset = NULL,
  label = "inquiry",
  handler = NULL,
  draws = 1L
)

declare_inquiries(
  ...,
  subset = NULL,
  label = "inquiry",
  handler = NULL,
  draws = 1L
)

declare_estimand(
  ...,
  subset = NULL,
  label = "inquiry",
  handler = NULL,
  draws = 1L
)

declare_estimands(
  ...,
  subset = NULL,
  label = "inquiry",
  handler = NULL,
  draws = 1L
)
```

## Arguments

- ...:

  (optional) Named expressions. Each expression becomes an inquiry whose
  numeric value is recorded in the simulation output. An unnamed
  expression takes the step's `label` as its name.

- subset:

  (optional) Unquoted expression evaluated on the data; only rows where
  it is `TRUE` enter the estimands.

- label:

  (optional) Step label. Defaults to `"inquiry"`. A step that declares
  exactly one named inquiry takes that name as its label instead,
  overriding `label`.

- handler:

  (optional) A function. When supplied, `...` arguments are evaluated
  against the data and passed to `handler()` rather than being treated
  as named scalar inquiries; useful for vectorised inquiry sets (for
  example, `handler = tibble`).

- draws:

  (optional) Number of nested draws for this step. Defaults to `1`. When
  `> 1`, the step is re-executed `draws` times for each upstream draw
  during nested simulation.

## Value

A `design_step` (class `c("design_step", "dd", "function")`): a function
that takes a data frame and returns a tibble with columns `inquiry` and
`estimand`, one row per inquiry. Add it to a design with `+`.

## See also

Other design declarations:
[`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md),
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md),
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
  declare_model(N = 100, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))

draw_estimands(design)
#> # A tibble: 1 × 2
#>   inquiry estimand
#>   <chr>      <dbl>
#> 1 ATE          0.5

# Placed before declare_sampling(), the question is about the population;
# placed after, about the sample.
design <-
  declare_model(N = 500, Y = rnorm(N)) +
  declare_inquiry(population_mean = mean(Y)) +
  declare_sampling(S = sample(rep(0:1, length.out = N))) +
  declare_inquiry(sample_mean = mean(Y))

draw_estimands(design)
#> # A tibble: 2 × 2
#>   inquiry         estimand
#>   <chr>              <dbl>
#> 1 population_mean -0.00338
#> 2 sample_mean     -0.00732
```
