# Print a design

Lists the steps as the calls that declared them, then the parameters and
objects the design's expressions refer to: the names
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
can change, the value each one currently holds, its kind, and which
steps would respond to a change. A design with no such names prints its
steps alone. [`summary()`](https://rdrr.io/r/base/summary.html) runs the
design once as well and says what each step did to the data.

## Usage

``` r
# S3 method for class 'design'
print(x, ...)
```

## Arguments

- x:

  (required) A `design`.

- ...:

  (optional) Ignored.

## Value

`x`, invisibly.

## Examples

``` r
d <- declare_model(N = 30, Y = rnorm(N)) + declare_inquiry(mu = mean(Y))
print(d)
#> Research design with 2 steps
#> 
#> Step 1 (model): declare_model(N = 30, Y = rnorm(N))
#> Step 2 (inquiry): declare_inquiry(mu = mean(Y))
```
