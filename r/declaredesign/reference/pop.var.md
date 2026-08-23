# Population variance function

Population variance function

## Usage

``` r
pop.var(x, na.rm = FALSE)
```

## Arguments

- x:

  a numeric vector, matrix or data frame.

- na.rm:

  logical. Should missing values be removed?

## Value

numeric scalar of the population variance

## Examples

``` r
x <- 1:4
var(x) # divides by (n-1)
#> [1] 1.666667
pop.var(x) # divides by n
#> [1] 1.25

```
