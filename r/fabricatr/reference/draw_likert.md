# Recode a latent variable into a Likert response variable

Recode a latent variable into a Likert response variable

## Usage

``` r
draw_likert(
  x,
  min = NULL,
  max = NULL,
  bins = NULL,
  breaks = NULL,
  labels = NULL
)
```

## Arguments

- x:

  a numeric variable considered to be "latent"

- min:

  the minimum value of the latent variable

- max:

  the maximum value of the latent variable

- bins:

  the number of Likert scale values. The latent variable will be cut
  into equally sized bins as in seq(min, max, length.out = bins + 1)

- breaks:

  A vector of breaks. This option is useful for settings in which
  equally-sized breaks are inappropriate

- labels:

  An optional vector of labels. If labels are provided, the resulting
  output will be a factor.

## Examples

``` r

x <- 1:100

draw_likert(x, min = 0, max = 100, bins = 7)
#>   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3
#>  [38] 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6
#>  [75] 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
draw_likert(x, breaks = c(-1, 10, 100))
#>   [1] 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#>  [38] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#>  [75] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

```
