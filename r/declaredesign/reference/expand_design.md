# Build a family of designs from a designer function (deprecated)

Deprecated in 2.0. Call the designer once and
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
the result: `redesign(designer(), N = c(100, 200))` does what
`expand_design(designer, N = c(100, 200))` did, and reaches the same
names, since a designer's argument is a name the design reads.
`expand_design()` still works and warns once per session.

## Usage

``` r
expand_design(.designer, ..., .expand = TRUE)
```

## Arguments

- .designer:

  (required) A function returning a `design`.

- ...:

  (optional) Named parameter values to vary, passed to `.designer` as
  its arguments.

- .expand:

  (optional) If `TRUE` (the default), expand the grid; if `FALSE`, zip
  parallel vectors.

## Value

A `design` or a named list of designs, as
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
returns.

## Details

The one thing it did that
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
cannot is rebuild a design whose *shape* depends on an argument, such as
a designer that adds a step when `blocked = TRUE`. For that, call the
designer with each value.

## See also

Other modifying a design:
[`delete_step()`](https://declaredesign.org/r/declaredesign/reference/delete_step.md),
[`design_notes()`](https://declaredesign.org/r/declaredesign/reference/design_notes.md),
[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md),
[`insert_step()`](https://declaredesign.org/r/declaredesign/reference/insert_step.md),
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md),
[`replace_step()`](https://declaredesign.org/r/declaredesign/reference/replace_step.md)

## Examples

``` r
designer <- function(N = 50) {
  declare_model(N = N, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
}
# deprecated
designs <- suppressWarnings(expand_design(designer, N = c(10, 20)))
# instead
designs <- redesign(designer(), N = c(10, 20))
length(designs)
#> [1] 2
```
