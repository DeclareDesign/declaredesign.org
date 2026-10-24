# Generate a variable correlated with an existing variable

Uses a Gaussian copula to produce a draw from `draw_handler` whose rank
correlation with `given` is approximately `rho`. Works with any `draw_*`
function that accepts a `quantile_y` argument, and with base R
random-number generators (e.g. `rnorm`, `rpois`).

## Usage

``` r
correlate(draw_handler, ..., given, rho)
```

## Arguments

- draw_handler:

  Unquoted function name. Either one of the three `draw_*` functions
  that take a `quantile_y` argument (`draw_binary`, `draw_binomial`,
  `draw_count`) or a base R `r*` generator with a `q*` counterpart
  (`rnorm`, `rpois`, `rbinom`, and the rest of the stats family).
  `draw_ordered`, `draw_likert`, `draw_categorical`, `draw_quantile`,
  and the two ICC draws take no quantile argument and are refused with a
  message saying so.

- ...:

  Arguments forwarded to `draw_handler` (e.g. `prob`, `mean`).

- given:

  Reference vector; the new variable will be rank-correlated with this.

- rho:

  Correlation of the Gaussian copula, in \\\[-1, 1\]\\. It is not the
  realized Spearman correlation, which is smaller by the copula's own
  relation, \\(6/\pi)\arcsin(\rho/2)\\: `rho = 0.7` lands at 0.68 and
  `rho = 0.3` at 0.288. The realized Pearson correlation is `rho`.
  fabricatr 1.x behaves identically.

## Value

A double vector of length `length(given)`. Note that the type is double
whatever `draw_handler` is: `draw_binary`, `draw_binomial`, and
`draw_count` each return an integer vector when called directly and a
double one when routed through here, because the copula reaches them
through a quantile function. The values are unaffected. An `NA` in
`given` yields an `NA` in the result at that position.

## Examples

``` r
score  <- rnorm(100, mean = 75, sd = 10)
offers <- correlate(draw_count, mean = 3, given = score, rho = 0.6)
cor(score, offers, method = "spearman")
#> [1] 0.6068749

# Works with base R generators too
y2 <- correlate(rnorm, mean = 0, sd = 1, given = score, rho = -0.5)
```
