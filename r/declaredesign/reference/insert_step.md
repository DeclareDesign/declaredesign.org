# Insert a step into a design (deprecated)

Deprecated in 2.0, and warns once per session. Reconstruct the design
with `+` instead.

## Usage

``` r
insert_step(design, new_step, before = NULL, after = NULL)
```

## Arguments

- design:

  (required) A `design`.

- new_step:

  (required) A `design_step` to insert.

- before, after:

  (required) A label, integer, or `design_step` indicating the anchor
  point. Provide exactly one of the two; if both are given, `after` is
  used. `before` comes first, as in DeclareDesign 1.x, so a script that
  passed the anchor by position keeps its meaning.

## Value

A `design` with `new_step` inserted at the anchor.

## See also

Other modifying a design:
[`delete_step()`](https://declaredesign.org/r/declaredesign/reference/delete_step.md),
[`design_notes()`](https://declaredesign.org/r/declaredesign/reference/design_notes.md),
[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md),
[`expand_design()`](https://declaredesign.org/r/declaredesign/reference/expand_design.md),
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md),
[`replace_step()`](https://declaredesign.org/r/declaredesign/reference/replace_step.md)

## Examples

``` r
design <- declare_model(N = 30, Y = rnorm(N)) +
  declare_inquiry(mu = mean(Y))
new_design <- insert_step(design,
  declare_measurement(Y2 = Y * 2),
  after = "model")
#> Warning: `insert_step()` is deprecated. Reconstruct the design explicitly instead.
#> This warning is displayed once per session.
names(new_design)
#> [1] "model"       "measurement" "mu"         
```
