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
#>   ID       noise noise_squared zero
#> 1  1 -0.07051319   0.004972111    0
#> 2  2  1.35951702   1.848286534    0
#> 3  3  1.45801543   2.125808985    0
#> 4  4  0.06262103   0.003921393    0
#> 5  5 -1.30734563   1.709152583    0
```
