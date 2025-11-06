# Declare potential outcomes

Deprecated. Please use the potential_outcomes function within a
declare_model declaration.

## Usage

``` r
declare_potential_outcomes(
  ...,
  handler = potential_outcomes_handler,
  label = NULL
)

potential_outcomes_internal.formula(
  formula,
  conditions = c(0, 1),
  assignment_variables = "Z",
  data,
  level = NULL,
  label = outcome_variable
)

potential_outcomes_internal.NULL(
  formula = stop("Not provided"),
  ...,
  data,
  level = NULL
)
```

## Arguments

- ...:

  arguments to be captured, and later passed to the handler

- handler:

  a tidy-in, tidy-out function

- label:

  a string describing the step

- formula:

  a formula to calculate potential outcomes as functions of assignment
  variables.

- conditions:

  see
  [`expand_conditions`](https://declaredesign.org/r/declaredesign/reference/expand_conditions.md).
  Provide values (e.g. `conditions = 1:4`) for a single assignment
  variable. If multiple assignment variables, provide named list (e.g.
  `conditions = list(Z1 = 0:1, Z2 = 0:1)`). Defaults to 0:1 if no
  conditions provided.

- assignment_variables:

  The name of the assignment variable. Generally not required as names
  are taken from `conditions`.

- data:

  a data.frame

- level:

  a character specifying a level of hierarchy for fabricate to calculate
  at

## Value

a function that returns a data.frame
