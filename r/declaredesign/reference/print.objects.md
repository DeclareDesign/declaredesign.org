# Print the objects table, one row per name

Prints the table
[`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md)
returns, and that `print(design)` shows under "Parameters and objects
the design refers to", aggregated to one row per name with five columns.
`name` is the name
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
can change. `value` is a display snippet of what it currently holds.
`kind` is how
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
reads a bare replacement: `scalar`, `vector`, and `list` supply one
design per element, while `data`, `function`, and `other` are single
values. `declared` is `TRUE` when a
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md)
step declares the name and `FALSE` when the design reads it from the
environment it was written in. `steps` lists the positions of the steps
that read the name, which are the steps that would respond to a change.

## Usage

``` r
# S3 method for class 'objects'
print(x, ...)
```

## Arguments

- x:

  (required) An `objects` table, as returned by
  [`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md).

- ...:

  (optional) Ignored.

## Value

`x`, invisibly.

## Details

The underlying table has one row per name per step and carries the
`quosure` and `env` columns that locate each binding; those are left out
of the print.

## Examples

``` r
tau <- 0.3
design <-
  declare_parameters(n_units = 100) +
  declare_model(N = n_units, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + tau) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N))
design_parameters(design)
#>     name value   kind declared steps
#>  n_units   100 scalar     TRUE  1, 2
#>      tau   0.3 scalar    FALSE     2
```
