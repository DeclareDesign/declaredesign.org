# Used to inherit roxygen docs

Used to inherit roxygen docs

## Usage

``` r
declare_internal_inherit_params(
  ...,
  handler = function(data, ...) data.frame(BLNK = "MSG", stringsAsFactors = TRUE),
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
