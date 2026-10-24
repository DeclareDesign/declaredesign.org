# Draw normally distributed data with serial correlation within clusters

Draws a first-order autoregressive (AR(1)) process separately inside
each cluster, the error structure of a panel in which a unit's shocks
persist from one period to the next. Within a cluster, rows are ordered
by `time`; the first value is a draw from the stationary distribution
and each later one is \$\$e_t = r e\_{t-1} + \sqrt{1 - r^2} \\ \sigma
u_t, \quad r = \rho^{\Delta t},\$\$ where \\\Delta t\\ is the gap in
`time` since the unit's previous observation and \\u_t\\ is standard
normal. Because the process starts at its stationary distribution, every
period has standard deviation `sd` and there is no burn-in. Two
observations of the same unit \\k\\ periods apart have correlation
\\\rho^k\\. With equally spaced periods this is the ordinary AR(1), and
with gaps it is the continuous-time version, so an unbalanced panel
needs no special handling. This is the process that
[`nlme::corAR1()`](https://rdrr.io/pkg/nlme/man/corAR1.html) and
[`nlme::corCAR1()`](https://rdrr.io/pkg/nlme/man/corCAR1.html) assume.

## Usage

``` r
draw_normal_ar(clusters, time, rho, mean = 0, sd = 1)
```

## Arguments

- clusters:

  Vector of unit IDs. Each unit gets its own independent series.

- time:

  Vector giving each row's position in its unit's series. Numeric, a
  `Date` (read in days, so `rho` is the correlation from one day to the
  next), or character or factor values that read as numbers, which is
  what
  [`cross_levels`](https://declaredesign.org/r/fabricatr/reference/cross_levels.md)
  produces for a level's ID. A factor is read by its labels, never by
  the order of its levels. A unit may not have two rows at the same
  time.

- rho:

  Correlation between consecutive periods, strictly between -1 and 1. A
  negative `rho` needs whole-number gaps in `time`, since \\\rho^{\Delta
  t}\\ is undefined otherwise.

- mean:

  Mean, a scalar or one value per row. Default 0.

- sd:

  Standard deviation of the process in every period. Default 1. This is
  the marginal standard deviation, the spread of the values in any one
  period, so changing `rho` changes persistence and leaves the variance
  alone. [`stats::arima.sim()`](https://rdrr.io/r/stats/arima.sim.html)
  takes the standard deviation of each period's new shock instead, which
  is `sd * sqrt(1 - rho^2)` here: the same number there gives a process
  `1 / sqrt(1 - rho^2)` times wider, 2.3 times at `rho = 0.9`.

## Value

A double vector of the same length as `clusters`, in the order the rows
were given.

## Details

The result is a latent normal variable. For a binary or count outcome
with the same persistence, pass it on: as `latent` to
[`draw_binary`](https://declaredesign.org/r/fabricatr/reference/draw_binary.md)
with a probit link, or as `quantile_y = pnorm(e)` to
[`draw_count`](https://declaredesign.org/r/fabricatr/reference/draw_count.md)
and the other `draw_*` functions. The serial correlation of the outcome
is then weaker than `rho`, as with
[`correlate`](https://declaredesign.org/r/fabricatr/reference/correlate.md).

The function draws persistent errors, not state dependence: a model in
which \\Y_t\\ depends on \\Y\_{t-1}\\ itself is a different model.

## Examples

``` r
panel <- fabricate(
  units = declare_level(N = 50, u = rnorm(N)),
  periods = declare_level(N = 10),
  obs = cross_levels(
    .by = c("units", "periods"),
    e = draw_normal_ar(clusters = units, time = periods, rho = 0.7),
    Y = u + e
  )
)

# the lag-1 correlation of e within unit is close to 0.7
panel <- panel[order(panel$units, as.numeric(panel$periods)), ]
same_unit <- panel$units[-1] == panel$units[-nrow(panel)]
cor(panel$e[-1][same_unit], panel$e[-nrow(panel)][same_unit])
#> [1] 0.6924371
```
