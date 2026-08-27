# Declare sampling procedure

Add a step to a design that indicates which variables from a population
get sampled and then passed on for estimation. Sampling is a part of the
data strategy of a research design.

## Usage

``` r
declare_sampling(..., handler = sampling_handler, label = NULL)

sampling_handler(data, ..., legacy = FALSE)
```

## Arguments

- ...:

  arguments to be captured, and later passed to the handler

- handler:

  a tidy-in, tidy-out function

- label:

  a string describing the step

- data:

  A data.frame.

- legacy:

  Use the legacy randomizr functionality. This will be disabled in
  future; please use legacy = FALSE.

## Value

A sampling declaration, which is a function that takes a data.frame as
an argument and returns a data.frame subsetted to sampled observations
and (optionally) augmented with inclusion probabilities and other
quantities.

## Examples

``` r
 
# declare_sampling in use
## Two-arm randomized experiment
design <-
  declare_model(
    N = 500,
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N = N, n = 200)) +
  declare_assignment(Z = complete_ra(N = N, m = 100)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")
  
run_design(design)
#>   inquiry estimand estimator term  estimate  std.error statistic      p.value
#> 1     ATE      0.2 estimator    Z 0.2863781 0.07900873  3.624638 0.0003678948
#>    conf.low conf.high  df outcome
#> 1 0.1305714 0.4421847 198       Y

# Set up population to sample from
model <- declare_model(
  villages = add_level(
    N = 30, 
    N_households = sample(c(50:100), N, replace = TRUE)
  ),
  households = add_level(
    N = N_households, 
    N_members = sample(c(1, 2, 3, 4), N, 
                       prob = c(0.2, 0.3, 0.25, 0.25), replace = TRUE)
  ),
  individuals = add_level(
    N = N_members, 
    age = sample(18:90, N, replace = TRUE),
    gender = rbinom(n = N, size = 1, prob = .5)
  )
)

# Sampling procedures
## Complete random sampling
design <- model +
  declare_sampling(S = complete_rs(N = N, n = 1000))
  
head(draw_data(design))
#>    villages N_households households N_members individuals age gender S
#> 1        01           64       0001         1        0001  23      1 1
#> 2        01           64       0002         4        0002  76      0 1
#> 7        01           64       0003         2        0007  23      0 1
#> 8        01           64       0004         3        0008  23      1 1
#> 25       01           64       0010         4        0025  40      1 1
#> 28       01           64       0012         2        0028  46      1 1

## Cluster random sampling
design <- model +
  declare_sampling(S = cluster_rs(clusters = villages, 
                                  n = 15))

head(draw_data(design))
#>     villages N_households households N_members individuals age gender S
#> 191       02           86       0075         4        0191  81      1 1
#> 192       02           86       0075         4        0192  30      0 1
#> 193       02           86       0075         4        0193  84      0 1
#> 194       02           86       0075         4        0194  37      1 1
#> 195       02           86       0076         1        0195  33      0 1
#> 196       02           86       0077         3        0196  48      1 1

## Strata and cluster random sampling
design <- model +
  declare_sampling(S  = strata_and_cluster_rs(
    strata = villages,
    clusters = households,
    strata_n = rep(20, 30)))
    
head(draw_data(design))
#>    villages N_households households N_members individuals age gender S
#> 9        01           93       0004         2        0009  67      1 1
#> 10       01           93       0004         2        0010  51      1 1
#> 11       01           93       0005         1        0011  85      0 1
#> 21       01           93       0009         4        0021  62      1 1
#> 22       01           93       0009         4        0022  24      1 1
#> 23       01           93       0009         4        0023  90      1 1

## Stratified random sampling
design <- model +
  declare_sampling(S = strata_rs(strata = gender, n = 100))

head(draw_data(design))
#>    villages N_households households N_members individuals age gender S
#> 2        01           88       0001         2        0002  47      1 1
#> 6        01           88       0003         2        0006  42      0 1
#> 12       01           88       0005         2        0012  36      0 1
#> 13       01           88       0005         2        0013  46      1 1
#> 51       01           88       0020         3        0051  51      0 1
#> 77       01           88       0031         3        0077  53      0 1
```
