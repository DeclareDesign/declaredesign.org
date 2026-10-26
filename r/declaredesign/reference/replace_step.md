# Replace a step in a design (deprecated)

Deprecated in 2.0, and warns once per session. Reconstruct the design
with `+` instead.

## Usage

``` r
replace_step(design, step, new_step)
```

## Arguments

- design:

  (required) A `design`.

- step:

  (required) A label, integer, or `design_step` naming the step to
  replace.

- new_step:

  (required) The replacement `design_step`.

## Value

A `design` with `new_step` in place of `step`.

## See also

Other modifying a design:
[`delete_step()`](https://declaredesign.org/r/declaredesign/reference/delete_step.md),
[`design_notes()`](https://declaredesign.org/r/declaredesign/reference/design_notes.md),
[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md),
[`expand_design()`](https://declaredesign.org/r/declaredesign/reference/expand_design.md),
[`insert_step()`](https://declaredesign.org/r/declaredesign/reference/insert_step.md),
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)

## Examples

``` r
design <- declare_model(N = 30, Y = rnorm(N)) +
  declare_inquiry(mu = mean(Y))
replace_step(design, "mu", declare_inquiry(med = median(Y)))
#> Warning: `replace_step()` is deprecated. Reconstruct the design explicitly instead.
#> This warning is displayed once per session.
#> Research design with 2 steps
#> 
#> Step 1 (model): declare_model(N = 30, Y = rnorm(N))
#> Step 2 (inquiry): declare_inquiry(med = median(Y))
```
