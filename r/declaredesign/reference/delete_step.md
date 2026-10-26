# Delete a step from a design (deprecated)

Deprecated in 2.0, and warns once per session. Reconstruct the design
with `+` instead.

## Usage

``` r
delete_step(design, step)
```

## Arguments

- design:

  (required) A `design`.

- step:

  (required) A label, integer, or `design_step` naming the step to
  delete.

## Value

A `design` without that step.

## See also

Other modifying a design:
[`design_notes()`](https://declaredesign.org/r/declaredesign/reference/design_notes.md),
[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md),
[`expand_design()`](https://declaredesign.org/r/declaredesign/reference/expand_design.md),
[`insert_step()`](https://declaredesign.org/r/declaredesign/reference/insert_step.md),
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md),
[`replace_step()`](https://declaredesign.org/r/declaredesign/reference/replace_step.md)

## Examples

``` r
design <- declare_model(N = 30, Y = rnorm(N)) +
  declare_inquiry(mu = mean(Y))
delete_step(design, "mu") |> names()
#> Warning: `delete_step()` is deprecated. Reconstruct the design explicitly instead.
#> This warning is displayed once per session.
#> [1] "model"
```
