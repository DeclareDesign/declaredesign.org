# Name the levels to cross or link

fabricatr 1.x's helper for \`by = join_using(countries, years)\`. In 2.0
the levels are named directly, \`.by = c("countries", "years")\`, and
this is kept so designs written for 1.x run unchanged. It returns
exactly that character vector, so it is also harmless in front of
\`.by\`.

## Usage

``` r
join_using(...)
```

## Arguments

- ...:

  Bare level names, or strings.

## Value

A character vector of level names.

## Examples

``` r
join_using(countries, years)
#> [1] "countries" "years"    
```
