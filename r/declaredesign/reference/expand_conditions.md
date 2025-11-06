# Expand assignment conditions

Internal helper to eagerly build assignment conditions for potential
outcomes.

## Usage

``` r
expand_conditions(
  formula,
  conditions = c(0, 1),
  assignment_variables = "Z",
  data,
  level = NULL,
  label = NULL
)
```

## Arguments

- conditions:

  the conditions

- assignment_variables:

  the name of assignment variables, if conditions is not already named.

## Value

a data.frame of potential outcome conditions

## Details

If conditions is a data.frame, it is returned unchanged

Otherwise, if conditions is a list, it is passed to expand.grid for
expansion to a data.frame

Otherwise, if condition is something else, box it in a list with
assignment_variables for names, and pass that to expand.grid.
