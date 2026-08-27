# Modify internal variables in a design

Internal helper for \`redesign()\`. Updates variable values inside
quosure environments of a design object so that rerunning the design
reflects new inputs.

## Usage

``` r
modify_edit(design, ...)
```

## Arguments

- design:

  A design object created by DeclareDesign.

- ...:

  Named variable updates (e.g., \`N = 20\`, \`b = 0.5\`).

## Value

A design with updated variables.

## Examples

``` r
n <- 2
b <- 1
d <- declare_model(N = n, Y = rnorm(N, b)) + declare_inquiry(Q = b)
d2 <- redesign(d, n = 3, b = 0.2)
draw_data(d2)
#>   ID          Y
#> 1  1 1.03764855
#> 2  2 0.03435161
#> 3  3 0.76054149
```
