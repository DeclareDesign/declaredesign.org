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
```

## Arguments

- ...:

  arguments to be captured, and later passed to the handler

- handler:

  a tidy-in, tidy-out function

- label:

  a string describing the step

## Value

a function that returns a data.frame
