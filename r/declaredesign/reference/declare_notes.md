# Declare quantities computed while a design runs

Records a named quantity at a point in the pipeline and makes it
available to every step that follows. A note is computed when the design
runs, at the position it is declared, from whatever is in scope there:
the parameters, the notes already taken, and the data as they stand at
that moment.

    declare_model(N = 100, Y = rnorm(N)) +
      declare_notes(tallest = max(Y)) +
      declare_sampling(S = complete_rs(N, n = 10)) +
      declare_inquiry(shortfall = mean(tallest - Y))

`tallest` is the largest value in the population, and the inquiry reads
it after sampling has thrown most of that population away. Notes are
what carry a quantity across a step that destroys the data it was
computed from.

## Usage

``` r
declare_notes(..., label = "notes")
```

## Arguments

- ...:

  (optional) Named quantities. Each is evaluated when the design runs,
  in order, and may read the parameters, the notes taken before it, and
  the current data.

- label:

  (optional) Step label. Defaults to `"notes"`.

## Value

A `design_step` (class `c("design_step", "dd", "function")`) whose
`dots` attribute holds the note expressions. Called on a data frame, it
returns the data unchanged. Add it to a design with `+`.

## Details

A note is fixed from the moment it is taken until the end of that run.
It is not recomputed by the steps that read it, so `tallest` above stays
the population maximum rather than becoming the sample maximum. It is
redrawn on the next run, because the data are. Declaring the same name
again later overwrites it from that point on and leaves the earlier
steps alone.

A note that reads no data is the ordinary way to write a quantity
derived from a parameter:

    declare_parameters(m_arms = 3) +
      declare_notes(ks = seq_len(m_arms)[-1],
                    term_names = paste0("factor(Z)", ks))

`m_arms` is the one thing a caller sets: `redesign(design, m_arms = 4)`.
`ks` and `term_names` follow from it, and cannot be set independently
without contradicting it, so
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
refuses them and
[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md)
does not list them. That is the difference between a note and a
parameter. A
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md)
step names what a caller may change; a `declare_notes()` step names what
the design works out for itself.

Within one call, each note may read the ones declared before it. A note
may not share a name with a declared parameter.

The step generates no data and adds no rows to a design's output.

## See also

[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md)
for the quantities a caller may change,
[`design_notes()`](https://declaredesign.org/r/declaredesign/reference/design_notes.md)
for the notes a design takes.

Other design declarations:
[`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md),
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md),
[`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md),
[`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md),
[`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md),
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md),
[`declare_sampling()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md),
[`declare_step()`](https://declaredesign.org/r/declaredesign/reference/declare_step.md),
[`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)

## Examples

``` r
# A quantity that outlives the data it is computed from.
design <-
  declare_model(N = 100, Y = rnorm(N)) +
  declare_notes(population_mean = mean(Y)) +
  declare_sampling(S = complete_rs(N, n = 10)) +
  declare_inquiry(gap = mean(Y) - population_mean)
draw_estimands(design)
#> # A tibble: 1 × 2
#>   inquiry estimand
#>   <chr>      <dbl>
#> 1 gap      -0.0459

# A quantity derived from a parameter, so `m_arms` is the only name a
# redesign can set.
design <-
  declare_parameters(m_arms = 3) +
  declare_notes(inquiry_names = paste0("ate_", seq_len(m_arms)[-1])) +
  declare_model(N = 60, u = rnorm(N)) +
  declare_inquiry(handler = function(data) {
    data.frame(inquiry = inquiry_names, estimand = 0)
  })
design_parameters(design)
#>    name value   kind declared steps
#>  m_arms     3 scalar     TRUE  1, 2
design_notes(design)
#>            name                                expr step
#> 1 inquiry_names paste0("ate_", seq_len(m_arms)[-1])    2
draw_estimands(redesign(design, m_arms = 4))
#> # A tibble: 3 × 2
#>   inquiry estimand
#>   <chr>      <dbl>
#> 1 ate_2          0
#> 2 ate_3          0
#> 3 ate_4          0
```
