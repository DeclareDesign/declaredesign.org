# Getting started

``` r

library(fabricatr)
```

fabricatr makes data you do not have yet. You describe the variables and
how they relate, and
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
builds a data frame that looks like what you expect to collect, so a
design can be run and diagnosed before there is anything to analyze.

## One level

[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
builds a data frame one column at a time. `N` is the number of rows, and
every column can use the columns before it:

``` r

survey <- fabricate(
  N = 100,
  age = round(runif(N, 18, 80)),
  income = 20 + 0.5 * age + rnorm(N, sd = 10)
)
head(survey)
#> # A tibble: 6 × 3
#>   ID      age income
#>   <chr> <dbl>  <dbl>
#> 1 001      23   21.9
#> 2 002      47   58.8
#> 3 003      61   48.4
#> 4 004      68   52.2
#> 5 005      53   52.2
#> 6 006      73   73.5
```

[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
adds a unit identifier as the first column, named `ID` unless you choose
another name with `ID_label`. IDs are character strings, zero-padded to
the width of the largest one, so these run `"001"` to `"100"` and a
frame of six rows runs `"1"` to `"6"`. `N = 100` sets the number of
rows, and the name `N` can then be used inside any column’s expression,
which is why `runif(N, 18, 80)` and `rnorm(N, sd = 10)` each draw
exactly one value per row.

## Two levels

Real data have structure: citizens in villages, students in schools,
years within countries. Each level is a named argument to
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md).
[`add_level()`](https://declaredesign.org/r/fabricatr/reference/add_level.md)
builds the first one, here five villages, each with its own
`village_wealth`.
[`nest_level()`](https://declaredesign.org/r/fabricatr/reference/nest_level.md)
then builds four citizens inside each village, twenty rows in all. Every
citizen row carries the `village_wealth` of the village it belongs to,
so the expression for `income` can use it:

``` r

nested <- fabricate(
  villages = add_level(N = 5, village_wealth = rnorm(N)),
  citizens = nest_level(N = 4, income = village_wealth + rnorm(N))
)
head(nested)
#> # A tibble: 6 × 4
#>   villages village_wealth citizens  income
#>   <chr>             <dbl> <chr>      <dbl>
#> 1 1                 0.192 01        0.0316
#> 2 1                 0.192 02        0.311 
#> 3 1                 0.192 03        0.810 
#> 4 1                 0.192 04       -0.848 
#> 5 2                 1.30  05        2.55  
#> 6 2                 1.30  06        1.62
```

`N` inside a level means the number of rows that level is building. In
`nest_level(N = 4, ...)` you supply 4 per village, but the expressions
inside see `N` as 20, so `rnorm(N)` gives each of the twenty citizens a
draw of their own. Each level names its own ID column after itself, here
`villages` and `citizens`, in place of `ID`.

## Potential outcomes

An experiment has an outcome under treatment and an outcome under
control, and observes only one of them.
[`potential_outcomes()`](https://declaredesign.org/r/fabricatr/reference/potential_outcomes.md)
writes both down from a formula, one column per condition. `Z` in the
formula is not a column: it names the treatment, and the formula is
evaluated once with `Z = 0` and once with `Z = 1`, giving `Y_Z_0` and
`Y_Z_1`:

``` r

experiment <- fabricate(
  N = 100,
  U = rnorm(N),
  potential_outcomes(Y ~ 0.5 * Z + U)
)
head(experiment)
#> # A tibble: 6 × 4
#>   ID        U Y_Z_0 Y_Z_1
#>   <chr> <dbl> <dbl> <dbl>
#> 1 001   0.702 0.702 1.20 
#> 2 002   0.152 0.152 0.652
#> 3 003   1.83  1.83  2.33 
#> 4 004   2.03  2.03  2.53 
#> 5 005   1.30  1.30  1.80 
#> 6 006   0.821 0.821 1.32
```

Once units are assigned,
[`reveal_outcomes()`](https://declaredesign.org/r/fabricatr/reference/reveal_outcomes.md)
picks out the outcome each unit shows. A call that starts from `data`
takes `N` from it, so `N` here is `nrow(experiment)`, 100:

``` r

observed <- fabricate(
  data = experiment,
  Z = rbinom(N, 1, 0.5),
  Y = reveal_outcomes(Y ~ Z)
)
head(observed)
#> # A tibble: 6 × 6
#>   ID        U Y_Z_0 Y_Z_1     Z     Y
#>   <chr> <dbl> <dbl> <dbl> <int> <dbl>
#> 1 001   0.702 0.702 1.20      1 1.20 
#> 2 002   0.152 0.152 0.652     1 0.652
#> 3 003   1.83  1.83  2.33      1 2.33 
#> 4 004   2.03  2.03  2.53      0 2.03 
#> 5 005   1.30  1.30  1.80      0 1.30 
#> 6 006   0.821 0.821 1.32      0 0.821
```

## Draws that look like real variables

The `draw_*` family turns a latent variable into the kind of measurement
a survey produces.
[`draw_binary()`](https://declaredesign.org/r/fabricatr/reference/draw_binary.md)
gives a 0/1 outcome. `latent` is the variable on the scale the link
works on, so with `link = "logit"` each unit is a 1 with probability
`plogis(x)`:

``` r

fabricate(
  N = 6,
  x = rnorm(N),
  voted = draw_binary(latent = x, link = "logit")
)
#> # A tibble: 6 × 3
#>   ID          x voted
#>   <chr>   <dbl> <int>
#> 1 1     -3.35       0
#> 2 2     -1.06       0
#> 3 3     -0.0204     0
#> 4 4      0.576      1
#> 5 5     -0.286      1
#> 6 6     -0.0579     1
```

[`draw_normal_icc()`](https://declaredesign.org/r/fabricatr/reference/draw_normal_icc.md)
draws a clustered variable with a target intra-cluster correlation, so
that citizens in the same village resemble each other by the amount you
choose:

``` r

clustered <- fabricate(
  villages = add_level(N = 100),
  citizens = nest_level(N = 20, trust = draw_normal_icc(clusters = villages, ICC = 0.3))
)
icc_share <- summary(lm(trust ~ villages, data = clustered))$adj.r.squared
icc_share
#> [1] 0.2495772
```

The adjusted figure is the one to read: it is the share of variance
lying between villages, which is what the ICC sets. The ICC is a
parameter of the draw rather than a property of any one data set, so the
realized share varies around it, with a standard deviation of about 0.03
across draws of a hundred villages. This draw lands at 0.25. Plain
`r.squared` runs about 0.03 higher still, because it counts the
ninety-nine village dummies as explanatory power.

[`draw_normal_ar()`](https://declaredesign.org/r/fabricatr/reference/draw_normal_ar.md)
is the same idea over time.
[`declare_level()`](https://declaredesign.org/r/fabricatr/reference/declare_level.md)
builds a level that does not nest in the one before it: here 2,000 units
and, separately, 6 periods.
[`cross_levels()`](https://declaredesign.org/r/fabricatr/reference/cross_levels.md)
then pairs every unit with every period, giving a panel of 12,000 rows,
one per unit per period, and `.by` names the two levels being crossed.
The draw then gives each unit a series whose shocks persist, so a unit
resembles its own recent past by the amount `rho` sets:

``` r

panel <- fabricate(
  units = declare_level(N = 2000),
  periods = declare_level(N = 6),
  obs = cross_levels(
    .by = c("units", "periods"),
    shock = draw_normal_ar(clusters = units, time = periods, rho = 0.7)
  )
)
head(panel)
#> # A tibble: 6 × 4
#>   units periods obs    shock
#>   <chr> <chr>   <chr>  <dbl>
#> 1 0001  1       00001 -0.306
#> 2 0002  1       00002 -1.38 
#> 3 0003  1       00003 -2.37 
#> 4 0004  1       00004  0.657
#> 5 0005  1       00005  0.596
#> 6 0006  1       00006  0.202
```

To see the persistence, put each period in its own column, one row per
unit, and correlate the first period with the rest:

``` r

wide <- tapply(panel$shock, list(panel$units, panel$periods), sum)
round(cor(wide)[1, ], 2)
#>    1    2    3    4    5    6 
#> 1.00 0.71 0.50 0.36 0.25 0.15
```

The correlation with the first period falls as the gap grows. In
expectation it is 0.7 one period later, 0.49 (0.7 squared) two periods
later, and in general 0.7 raised to the gap; a sample of 2,000 units
lands within a couple of hundredths of each.

## Resampling

[`resample_data()`](https://declaredesign.org/r/fabricatr/reference/resample_data.md)
bootstraps a data frame, and respects its levels: villages are drawn
with replacement, then citizens within each village that was drawn. A
village can come up more than once, and its citizens are resampled
separately each time.

``` r

boot <- resample_data(nested, N = c(villages = 3, citizens = 2))
boot
#> # A tibble: 6 × 4
#>   villages village_wealth citizens  income
#>   <chr>             <dbl> <chr>      <dbl>
#> 1 1                 0.192 02        0.311 
#> 2 1                 0.192 03        0.810 
#> 3 3                -0.559 11       -1.11  
#> 4 3                -0.559 12        0.0117
#> 5 1                 0.192 04       -0.848 
#> 6 1                 0.192 01        0.0316
```

`ALL` in place of a number keeps every unit at that level, and leaving a
level out of the plan does the same thing:

``` r

resample_data(nested, N = c(villages = 3, citizens = ALL))
#> # A tibble: 12 × 4
#>    villages village_wealth citizens income
#>    <chr>             <dbl> <chr>     <dbl>
#>  1 4                -0.513 13       -1.23 
#>  2 4                -0.513 14        0.380
#>  3 4                -0.513 15       -1.40 
#>  4 4                -0.513 16       -1.00 
#>  5 5                 2.42  17        3.29 
#>  6 5                 2.42  18        2.13 
#>  7 5                 2.42  19        1.63 
#>  8 5                 2.42  20        2.87 
#>  9 2                 1.30  05        2.55 
#> 10 2                 1.30  06        1.62 
#> 11 2                 1.30  07        1.74 
#> 12 2                 1.30  08        1.88
```

## Where to read next

The book at <https://book.declaredesign.org> puts fabricated data to
work in complete research designs, and
[`vignette("fabricatr2.0")`](https://declaredesign.org/r/fabricatr/articles/fabricatr2.0.md)
lists what changed from fabricatr 1.x for anyone with an older script.
Every function is documented in the [reference
index](https://declaredesign.org/r/fabricatr/reference/index.html).
