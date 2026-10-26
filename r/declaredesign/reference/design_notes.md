# Report the notes a design takes

Lists the names a
[`declare_notes()`](https://declaredesign.org/r/declaredesign/reference/declare_notes.md)
step declares and the expression each is computed from. Unlike
[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md),
no value is reported: a note has one only while the design is running,
and a note computed from data has a different one on every draw.

The two functions divide a design's names between them. What
[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md)
lists can be changed with
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md);
what `design_notes()` lists cannot, and follows from what can.

## Usage

``` r
design_notes(design)
```

## Arguments

- design:

  (required) A `design` or a `design_step`.

## Value

A data frame with one row per note and columns `name`, `expr` (the
expression, as a character label), and `step` (the position of the step
that declares it).

## See also

[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md).

Other modifying a design:
[`delete_step()`](https://declaredesign.org/r/declaredesign/reference/delete_step.md),
[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md),
[`expand_design()`](https://declaredesign.org/r/declaredesign/reference/expand_design.md),
[`insert_step()`](https://declaredesign.org/r/declaredesign/reference/insert_step.md),
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md),
[`replace_step()`](https://declaredesign.org/r/declaredesign/reference/replace_step.md)

## Examples

``` r
design <- declare_parameters(m_arms = 3) +
  declare_notes(ks = seq_len(m_arms)[-1]) +
  declare_model(N = 60)
design_notes(design)
#>   name                expr step
#> 1   ks seq_len(m_arms)[-1]    2
```
