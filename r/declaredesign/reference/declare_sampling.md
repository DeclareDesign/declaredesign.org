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
#> 1     ATE      0.2 estimator    Z 0.3447803 0.07842908  4.396078 1.796759e-05
#>    conf.low conf.high  df outcome
#> 1 0.1901168 0.4994438 198       Y

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
#> 1        01           72       0001         2        0001  25      1 1
#> 8        01           72       0003         3        0008  88      0 1
#> 9        01           72       0004         1        0009  71      1 1
#> 12       01           72       0006         2        0012  21      0 1
#> 29       01           72       0016         2        0029  28      0 1
#> 33       01           72       0017         4        0033  50      1 1

## Cluster random sampling
design <- model +
  declare_sampling(S = cluster_rs(clusters = villages, 
                                  n = 15))

head(draw_data(design))
#>     villages N_households households N_members individuals age gender S
#> 318       03           53       0132         1        0318  70      1 1
#> 319       03           53       0133         4        0319  41      0 1
#> 320       03           53       0133         4        0320  67      0 1
#> 321       03           53       0133         4        0321  72      1 1
#> 322       03           53       0133         4        0322  83      1 1
#> 323       03           53       0134         4        0323  26      0 1

## Strata and cluster random sampling
design <- model +
  declare_sampling(S  = strata_and_cluster_rs(
    strata = villages,
    clusters = households,
    strata_n = rep(20, 30)))
    
head(draw_data(design))
#>    villages N_households households N_members individuals age gender S
#> 9        01           62       0003         4        0009  87      0 1
#> 10       01           62       0003         4        0010  70      0 1
#> 11       01           62       0003         4        0011  55      1 1
#> 12       01           62       0003         4        0012  35      1 1
#> 18       01           62       0007         3        0018  20      0 1
#> 19       01           62       0007         3        0019  89      1 1

## Stratified random sampling
design <- model +
  declare_sampling(S = strata_rs(strata = gender, n = 100))

head(draw_data(design))
#>     villages N_households households N_members individuals age gender S
#> 50        01           61       0020         2        0050  55      0 1
#> 51        01           61       0021         1        0051  70      0 1
#> 68        01           61       0028         4        0068  38      1 1
#> 84        01           61       0034         4        0084  90      0 1
#> 100       01           61       0041         4        0100  29      1 1
#> 121       01           61       0049         2        0121  56      0 1
```
