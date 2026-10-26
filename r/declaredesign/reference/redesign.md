# Redesign

Returns a copy of the design with one or more parameters changed, so
that versions can be diagnosed side by side. Several values for a
parameter give several designs: with `.expand = TRUE` (the default) the
cross-product of the values is taken, and with `.expand = FALSE` they
are zipped position-wise.

## Usage

``` r
redesign(.design, ..., .expand = TRUE)
```

## Arguments

- .design:

  (required) A `design`. Named with a dot, like `.method` and `.summary`
  in
  [`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md),
  because everything else here is a parameter of the user's design: a
  plain `design` would partially match and swallow a parameter named
  `d`, `de`, `des`, `desi`, or `desig`, and designs with a parameter
  named `d` exist.

- ...:

  (optional) Named parameter values. A bare atomic vector, and a bare
  list, supply one design per element; a data frame, a matrix, a
  function, or any classed object is one value. To sweep over such
  values, pass a list of them, and to supply one list as a single value
  wrap it: `list(x)`.

- .expand:

  (optional) If `TRUE` (the default), expand the parameter grid; if
  `FALSE`, zip parallel vectors. Dotted for the same reason as
  `.design`: an undotted `expand` after the dots would collide exactly
  with a parameter of that name.

## Value

A single `design` if one combination is supplied, otherwise a list of
designs named by the values that distinguish them:
`redesign(design, N = c(10, 20))` gives `N = 10` and `N = 20`, so a
simulation's `design` column, and `bind_rows(.id = "design")`, carry the
values that vary. Designs are named `design_1`, `design_2`, etc. when a
parameter's value cannot be written down, such as a function or a data
frame.

## Details

A parameter that no step responds to draws a warning and is otherwise
ignored. [`summary()`](https://rdrr.io/r/base/summary.html) on a design
lists the names that are available.

An atomic vector always supplies one value per design, so a parameter
that is itself a vector has to be wrapped:
`prob_each = list(c(0, 0.5, 0.5))` is one design, where
`prob_each = c(0, 0.5, 0.5)` is three. Handing a bare vector to a
parameter that currently holds one warns.

A bare list is split the same way, one design per element, so a
parameter whose value is a list is wrapped by the same rule:
`levels_list = list(levels)` is one design and `levels_list = levels` is
one design per element of `levels`. The two are told apart by what the
elements are, since a sweep over lists holds a list in every element,
and a bare list handed to a list-valued parameter whose elements are not
lists warns.

Only bare vectors and bare lists are read that way. A data frame, a
matrix, and anything carrying a class are single replacement values, so
a design written as `declare_model(data = dataset, ...)`, with
`dataset <- pilot` in the workspace, swaps its data with
`redesign(design, dataset = real)` and needs no wrapping. Naming the
parameter `dataset` rather than after the data it starts with keeps the
name true once it is redesigned. A redesign reaches the name the design
reads the data under, not `data`, which names fabricate's argument and
belongs to the declaration.

## See also

Other modifying a design:
[`delete_step()`](https://declaredesign.org/r/declaredesign/reference/delete_step.md),
[`design_notes()`](https://declaredesign.org/r/declaredesign/reference/design_notes.md),
[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md),
[`expand_design()`](https://declaredesign.org/r/declaredesign/reference/expand_design.md),
[`insert_step()`](https://declaredesign.org/r/declaredesign/reference/insert_step.md),
[`replace_step()`](https://declaredesign.org/r/declaredesign/reference/replace_step.md)

## Examples

``` r
designer <- function(N) {
  declare_model(N = N, Y = rnorm(N)) +
    declare_inquiry(mu = mean(Y))
}
design <- designer(50)
redesigned <- redesign(design, N = c(10, 20))
length(redesigned)
#> [1] 2

# a function-valued parameter
summarizer <- function(x) mean(x)
design <- declare_model(N = 50, Y = rnorm(N)) +
  declare_inquiry(mu = summarizer(Y))
redesign(design, summarizer = stats::median)
#> Research design with 2 steps
#> 
#> Step 1 (model): declare_model(N = 50, Y = rnorm(N))
#> Step 2 (inquiry): declare_inquiry(mu = summarizer(Y))
#> 
#> Parameters and objects the design refers to:
#>        name                                    value     kind declared steps
#>  summarizer function (x, na.rm = FALSE, ...) UseM... function    FALSE     2
```
