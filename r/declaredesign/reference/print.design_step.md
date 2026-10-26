# Print a design step

Print a design step

## Usage

``` r
# S3 method for class 'design_step'
print(x, ...)
```

## Arguments

- x:

  (required) A `design_step`.

- ...:

  (optional) Ignored.

## Value

`x`, invisibly.

## Examples

``` r
s <- declare_model(N = 10, Y = rnorm(N))
print(s)
#> <design_step: model [model]>
```
