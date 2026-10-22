# Declare a custom step

With declare_step, you can include any function that takes data as one
of its arguments and returns data in a design declaration. The first
argument is always a "handler", which is the name of the data-in,
data-out function. For handy data manipulations use
`declare_step(fabricate, ...)`.

## Usage

``` r
declare_step(
  ...,
  handler = function(data, ...f, ...) ...f(data, ...),
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

A function that returns a data.frame.

## Examples

``` r

population <- declare_model(N = 5, noise = rnorm(N))
manipulate <- declare_step(fabricate, noise_squared = noise^2, zero = 0)

design <- population + manipulate
draw_data(design)
#> # A tibble: 5 × 4
#>   ID      noise noise_squared  zero
#>   <chr>   <dbl>         <dbl> <dbl>
#> 1 1     -0.488        0.238       0
#> 2 2      1.11         1.24        0
#> 3 3     -0.653        0.426       0
#> 4 4     -1.08         1.16        0
#> 5 5      0.0550       0.00302     0
```
