# Declare the size and features of the population

Deprecated. Please use declare_model instead.

## Usage

``` r
declare_population(..., handler = fabricate, label = NULL)
```

## Arguments

- ...:

  arguments to be captured, and later passed to the handler

- handler:

  a tidy-in, tidy-out function

- label:

  a string describing the step

## Value

A potential outcomes declaration, which is a function that returns a
data.frame.
