# Draw the realized data from a design

Runs the data-generating steps only. The inquiries and estimators are
not run, so a design whose estimator fails still draws its data, and
drawing data does not pay to fit models it would throw away.

## Usage

``` r
draw_data(design, ...)
```

## Arguments

- design:

  (required) A `design`.

- ...:

  (optional) Named parameter values, as in
  [`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md).
  Each supplies one value: use
  [`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
  itself to sweep over several. A parameter named `design`, or any
  prefix of it, cannot be supplied this way and needs the
  [`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
  route.

## Value

A tibble: the data as the last data-generating step leaves it.

## Details

Named parameter values in `...` are applied first, so
`draw_data(design, theta = 0.5)` is
`draw_data(redesign(design, theta = 0.5))`: a convenience for trying a
value without naming it above the design or rewriting the declaration.
The names reach the design by
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)'s
rule, so a name the design does not read warns and is ignored, and a
number written inside a step is refused with the two ways to name it.
`data`, `start` and `end`, 1.x's arguments for running part of a design
on supplied data, are refused with
[`get_estimates()`](https://declaredesign.org/r/declaredesign/reference/get_estimates.md)
and `design[start:end]` as the replacements, unless the design reads a
parameter by that name.

## See also

Other drawing from a design:
[`draw_estimands()`](https://declaredesign.org/r/declaredesign/reference/draw_estimands.md),
[`draw_estimates()`](https://declaredesign.org/r/declaredesign/reference/draw_estimates.md),
[`get_estimates()`](https://declaredesign.org/r/declaredesign/reference/get_estimates.md),
[`run_design()`](https://declaredesign.org/r/declaredesign/reference/run_design.md)

## Examples

``` r
design <- declare_model(N = 25, X = rnorm(N))
df <- draw_data(design)
nrow(df)
#> [1] 25

# A value the declaration leaves free, supplied at draw time
model <- declare_model(N = 25, D = rbinom(N, 1, 0.5), Y = theta * D + rnorm(N))
head(draw_data(model, theta = 0.5))
#> # A tibble: 6 × 3
#>   ID        D      Y
#>   <chr> <int>  <dbl>
#> 1 01        0 -0.805
#> 2 02        1 -1.43 
#> 3 03        0  0.717
#> 4 04        1  2.29 
#> 5 05        1  1.62 
#> 6 06        0  1.61 
```
