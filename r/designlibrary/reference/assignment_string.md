# Generates string of assignment of value to argument

Generates string of assignment of value to argument

## Usage

``` r
assignment_string(arg_name, arg_values)
```

## Arguments

- arg_name:

  A string. Label of assignment object.

- arg_values:

  A list. Values to be assigned to the argument. Can be character,
  logical or numeric of any length.

## Value

A vector of strings of the form `"arg_name <- arg_value"` where
`arg_value` is in its evaluated format.
