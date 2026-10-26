# Report the parameters a design can be redesigned on

Lists the names
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
can change: the parameters a
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md)
step declares, and the objects the design's expressions read out of the
environments they were written in.

Symbols that resolve to a package (`rnorm`, `complete_ra`) are not
parameters and are left out, and neither are names that resolve to
nothing, because those are columns an earlier step created.

## Usage

``` r
design_parameters(design)
```

## Arguments

- design:

  (required) A `design` or a `design_step`.

## Value

A data frame of class `objects` with one row per name per step: `name`,
`value` (a display snippet of the value), `kind` (`scalar`, `vector`,
`list`, `data`, `function`, or `other`), `declared` (whether a
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md)
step declares it), and `step` (the position of the step that reads it),
plus the list columns `quosure` and `env` that locate the binding. Rows
are in step order. It prints aggregated to one row per name.

## See also

Other modifying a design:
[`delete_step()`](https://declaredesign.org/r/declaredesign/reference/delete_step.md),
[`design_notes()`](https://declaredesign.org/r/declaredesign/reference/design_notes.md),
[`expand_design()`](https://declaredesign.org/r/declaredesign/reference/expand_design.md),
[`insert_step()`](https://declaredesign.org/r/declaredesign/reference/insert_step.md),
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md),
[`replace_step()`](https://declaredesign.org/r/declaredesign/reference/replace_step.md)

## Examples

``` r
design <- declare_parameters(n_units = 100) +
  declare_model(N = n_units, Y = rnorm(N))
design_parameters(design)
#>     name value   kind declared steps
#>  n_units   100 scalar     TRUE  1, 2
```
