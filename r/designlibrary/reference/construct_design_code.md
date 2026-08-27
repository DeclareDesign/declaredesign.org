# Generates clean code string that reproduces design

Generates clean code string that reproduces design

## Usage

``` r
construct_design_code(
  designer,
  args,
  args_to_fix = NULL,
  arguments_as_values = FALSE,
  exclude_args = NULL
)
```

## Arguments

- designer:

  Designer function.

- args:

  Named list of arguments to be passed to designer function.

- args_to_fix:

  Vector of strings. Designer arguments to fix in design code.

- arguments_as_values:

  Logical. Whether to replace argument names for value.

- exclude_args:

  Vector of strings. Name of arguments to be excluded from argument
  definition at top of design code.
