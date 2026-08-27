# Substitute approach

Substitute approach

## Usage

``` r
code_fixer(design_expr, list_fixed_str, eval_envir)
```

## Arguments

- design_expr:

  A string. The text of the expression in which you wish to substitute
  symbols for their set values.

- list_fixed_str:

  A string. The string of code that generates a named list of arguments
  that will be substituted in the evaluated `design_expr`.

- eval_envir:

  The evaluation environment. Defaults to environment in which design
  arguments are already evaluated.
