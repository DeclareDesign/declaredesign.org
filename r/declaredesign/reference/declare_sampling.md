# Declare sampling procedure

Declare sampling procedure

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
#>   inquiry estimand estimator term  estimate  std.error statistic    p.value
#> 1     ATE      0.2 estimator    Z 0.1371534 0.07941205   1.72711 0.08570732
#>      conf.low conf.high  df outcome
#> 1 -0.01944857 0.2937553 198       Y

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
#> 2        01           64       0001         3        0002  86      1 1
#> 10       01           64       0005         2        0010  65      0 1
#> 11       01           64       0005         2        0011  85      1 1
#> 24       01           64       0011         1        0024  73      0 1
#> 39       01           64       0019         3        0039  43      1 1
#> 45       01           64       0021         1        0045  62      0 1

## Cluster random sampling
design <- model +
  declare_sampling(S = cluster_rs(clusters = villages, 
                                  n = 15))

head(draw_data(design))
#>   villages N_households households N_members individuals age gender S
#> 1       01           65       0001         3        0001  18      1 1
#> 2       01           65       0001         3        0002  74      1 1
#> 3       01           65       0001         3        0003  38      1 1
#> 4       01           65       0002         1        0004  29      0 1
#> 5       01           65       0003         3        0005  24      0 1
#> 6       01           65       0003         3        0006  53      0 1

## Strata and cluster random sampling
design <- model +
  declare_sampling(S  = strata_and_cluster_rs(
    strata = villages,
    clusters = households,
    strata_n = rep(20, 30)))
    
head(draw_data(design))
#>    villages N_households households N_members individuals age gender S
#> 1        01           54       0001         1        0001  55      1 1
#> 6        01           54       0004         4        0006  21      0 1
#> 7        01           54       0004         4        0007  58      0 1
#> 8        01           54       0004         4        0008  30      0 1
#> 9        01           54       0004         4        0009  23      1 1
#> 12       01           54       0006         4        0012  86      1 1

## Stratified random sampling
design <- model +
  declare_sampling(S = strata_rs(strata = gender, n = 100))

head(draw_data(design))
#>     villages N_households households N_members individuals age gender S
#> 44        01           53       0020         4        0044  30      1 1
#> 56        01           53       0023         4        0056  20      0 1
#> 70        01           53       0030         3        0070  70      0 1
#> 103       01           53       0044         2        0103  29      1 1
#> 129       01           53       0052         3        0129  38      1 1
#> 150       02           96       0059         4        0150  63      1 1
```
