# Draw discrete variables including binary, binomial count, poisson count, ordered, and categorical

Drawing discrete data based on probabilities or latent traits is a
common task that can be cumbersome. Each function in our discrete
drawing set creates a different type of discrete data: `draw_binary`
creates binary 0/1 data, `draw_binomial` creates binomial data (repeated
trial binary data), `draw_categorical` creates categorical data,
`draw_ordered` transforms latent data into observed ordered categories,
`draw_count` creates count data (poisson-distributed).

## Usage

``` r
draw_binomial(
  prob = link(latent),
  trials = 1,
  N = length(prob),
  latent = NULL,
  link = "identity",
  quantile_y = NULL
)

draw_categorical(
  prob = link(latent),
  N = NULL,
  latent = NULL,
  link = "identity",
  category_labels = NULL
)

draw_ordered(
  x = link(latent),
  breaks = c(-1, 0, 1),
  break_labels = NULL,
  N = length(x),
  latent = NULL,
  strict = FALSE,
  link = "identity"
)

draw_count(
  mean = link(latent),
  N = length(mean),
  latent = NULL,
  link = "identity",
  quantile_y = NULL
)

draw_binary(
  prob = link(latent),
  N = length(prob),
  link = "identity",
  latent = NULL,
  quantile_y = NULL
)

draw_quantile(type, N)
```

## Arguments

- prob:

  A number or vector of numbers representing the probability for binary
  or binomial outcomes; or a number, vector, or matrix of numbers
  representing probabilities for categorical outcomes. If you supply a
  link function, these underlying probabilities will be transformed.

- trials:

  for `draw_binomial`, the number of trials for each observation

- N:

  number of units to draw. Defaults to the length of the vector of
  probabilities or latent data you provided.

- latent:

  If the user provides a link argument other than identity, they should
  provide the variable `latent` rather than `prob` or `mean`

- link:

  link function between the latent variable and the probability of a
  positive outcome, e.g. "logit", "probit", or "identity". For the
  "identity" link, the latent variable must be a probability.

- quantile_y:

  A vector of quantiles; if provided, rather than drawing stochastically
  from the distribution of interest, data will be drawn at exactly those
  quantiles.

- category_labels:

  vector of labels for the categories produced by `draw_categorical`. If
  provided, must be equal to the number of categories provided in the
  `prob` argument.

- x:

  for `draw_ordered`, the latent data for each observation.

- breaks:

  vector of breaks to cut a latent outcome into ordered categories with
  `draw_ordered`

- break_labels:

  vector of labels for the breaks to cut a latent outcome into ordered
  categories with `draw_ordered`. (Optional)

- strict:

  Logical indicating whether values outside the provided breaks should
  be coded as NA. Defaults to `FALSE`, in which case effectively
  additional breaks are added between -Inf and the lowest break and
  between the highest break and Inf.

- mean:

  for `draw_count`, the mean number of count units for each observation

- type:

  The number of buckets to split data into. For a median split, enter 2;
  for terciles, enter 3; for quartiles, enter 4; for quintiles, 5; for
  deciles, 10.

## Value

A vector of data in accordance with the specification; generally numeric
but for some functions, including `draw_ordered` and `draw_categorical`,
may be factor if labels are provided.

## Details

For variables with intra-cluster correlations, see
[`draw_binary_icc`](https://declaredesign.org/r/fabricatr/reference/draw_binary_icc.md)
and
[`draw_normal_icc`](https://declaredesign.org/r/fabricatr/reference/draw_normal_icc.md)

## Examples

``` r

# Drawing binary values (success or failure, treatment assignment)
fabricate(N = 3,
   p = c(0, .5, 1),
   binary = draw_binary(prob = p))
#>   ID   p binary
#> 1  1 0.0      0
#> 2  2 0.5      0
#> 3  3 1.0      1

# Drawing binary values with probit link (transforming continuous data
# into a probability range).
fabricate(N = 3,
   x = 10 * rnorm(N),
   binary = draw_binary(latent = x, link = "probit"))
#>   ID         x binary
#> 1  1 -6.109455      0
#> 2  2 12.518842      1
#> 3  3  2.503195      1

# Repeated trials: `draw_binomial`
fabricate(N = 3,
   p = c(0, .5, 1),
   binomial = draw_binomial(prob = p, trials = 10))
#>   ID   p binomial
#> 1  1 0.0        0
#> 2  2 0.5        4
#> 3  3 1.0       10

# Ordered data: transforming latent data into observed, ordinal data.
# useful for survey responses.
fabricate(N = 3,
   x = 5 * rnorm(N),
   ordered = draw_ordered(x = x,
                          breaks = c(-Inf, -1, 1, Inf)))
#>   ID         x ordered
#> 1  1  4.333357       3
#> 2  2  3.080771       3
#> 3  3 -7.412788       1

# Providing break labels for latent data.
fabricate(N = 3,
   x = 5 * rnorm(N),
   ordered = draw_ordered(x = x,
                          breaks = c(-Inf, -1, 1, Inf),
                          break_labels = c("Not at all concerned",
                                           "Somewhat concerned",
                                           "Very concerned")))
#>   ID          x              ordered
#> 1  1  0.6809368   Somewhat concerned
#> 2  2  6.4749434       Very concerned
#> 3  3 -6.4532501 Not at all concerned


# Count data: useful for rates of occurrences over time.
fabricate(N = 5,
   x = c(0, 5, 25, 50, 100),
   theft_rate = draw_count(mean=x))
#>   ID   x theft_rate
#> 1  1   0          0
#> 2  2   5          3
#> 3  3  25         25
#> 4  4  50         53
#> 5  5 100        113

# Categorical data: useful for demographic data.
fabricate(N = 6, p1 = runif(N), p2 = runif(N), p3 = runif(N),
          cat = draw_categorical(cbind(p1, p2, p3)))
#>   ID        p1         p2        p3 cat
#> 1  1 0.3385308 0.30210861 0.5196348   3
#> 2  2 0.8270794 0.62393433 0.1792204   1
#> 3  3 0.7520461 0.24698135 0.5736056   2
#> 4  4 0.2347526 0.09599854 0.6285673   3
#> 5  5 0.3393005 0.18790346 0.2713756   1
#> 6  6 0.3458746 0.03519621 0.5784195   3
```
