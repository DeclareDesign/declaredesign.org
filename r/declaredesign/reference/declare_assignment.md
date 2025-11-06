# Declare Data Strategy: Assignment

Declare Data Strategy: Assignment

## Usage

``` r
declare_assignment(..., handler = assignment_handler, label = NULL)

assignment_handler(data, ..., legacy = FALSE)
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

A function that takes a data.frame as an argument and returns a
data.frame with assignment columns appended.

## Examples

``` r
# declare_assignment in use
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
#>   inquiry estimand estimator term    estimate  std.error  statistic   p.value
#> 1     ATE      0.2 estimator    Z 0.006027208 0.07908535 0.07621143 0.9393278
#>     conf.low conf.high  df outcome
#> 1 -0.1499305 0.1619849 198       Y

# Set up population to assign
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

# Assignment procedures
## Complete random assignment
design <-
  model +
  declare_assignment(Z = complete_ra(N = N, m = 1000))
  
head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z
#> 1       01           88       0001         3        0001  33      0 0
#> 2       01           88       0001         3        0002  34      0 0
#> 3       01           88       0001         3        0003  77      1 0
#> 4       01           88       0002         1        0004  34      0 0
#> 5       01           88       0003         2        0005  58      0 0
#> 6       01           88       0003         2        0006  90      0 0

## Cluster random assignment
design <-
  model +
  declare_assignment(Z = cluster_ra(clusters = villages,
                                    n = 15))
                                    
head(draw_data(design))
#>   villages N_households households N_members individuals age gender   Z
#> 1       01           72       0001         2        0001  36      1 T15
#> 2       01           72       0001         2        0002  52      0 T15
#> 3       01           72       0002         1        0003  22      1 T15
#> 4       01           72       0003         2        0004  81      0 T15
#> 5       01           72       0003         2        0005  57      1 T15
#> 6       01           72       0004         1        0006  69      1 T15

## Block and cluster random assignment
design <-
  model +
  declare_assignment(Z  = block_and_cluster_ra(
    blocks = villages,
    clusters = households,
    block_m = rep(20, 30)
  ))

head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z
#> 1       01           94       0001         4        0001  84      1 0
#> 2       01           94       0001         4        0002  59      0 0
#> 3       01           94       0001         4        0003  78      1 0
#> 4       01           94       0001         4        0004  25      0 0
#> 5       01           94       0002         1        0005  67      1 0
#> 6       01           94       0003         4        0006  70      0 0

## Block random assignment
design <-
  model +
  declare_assignment(Z = block_ra(blocks = gender, m = 100))
  
head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z
#> 1       01           82       0001         4        0001  86      0 1
#> 2       01           82       0001         4        0002  74      1 0
#> 3       01           82       0001         4        0003  56      0 0
#> 4       01           82       0001         4        0004  26      1 0
#> 5       01           82       0002         2        0005  72      0 0
#> 6       01           82       0002         2        0006  88      1 0

## Block random assignment using probabilities
design <-
  model +
  declare_assignment(Z = block_ra(blocks = gender,
                                  block_prob = c(1 / 3, 2 / 3)))

head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z
#> 1       01           89       0001         2        0001  78      0 1
#> 2       01           89       0001         2        0002  19      1 1
#> 3       01           89       0002         3        0003  84      0 1
#> 4       01           89       0002         3        0004  42      0 1
#> 5       01           89       0002         3        0005  39      1 1
#> 6       01           89       0003         4        0006  62      1 0

## Factorial assignment
design <-
  model +
  declare_assignment(Z1 = complete_ra(N = N, m = 100),
                     Z2 = block_ra(blocks = Z1))

head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z1 Z2
#> 1       01           81       0001         4        0001  86      1  0  1
#> 2       01           81       0001         4        0002  85      0  0  0
#> 3       01           81       0001         4        0003  63      0  0  1
#> 4       01           81       0001         4        0004  31      0  0  1
#> 5       01           81       0002         3        0005  34      0  0  1
#> 6       01           81       0002         3        0006  24      1  0  1

## Assignment using functions outside of randomizr
design <-
  model +
  declare_assignment(Z = rbinom(n = N, size = 1, prob = 0.35))

head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z
#> 1       01           55       0001         3        0001  50      0 1
#> 2       01           55       0001         3        0002  31      1 1
#> 3       01           55       0001         3        0003  43      1 0
#> 4       01           55       0002         4        0004  19      1 1
#> 5       01           55       0002         4        0005  38      0 1
#> 6       01           55       0002         4        0006  19      1 1
```
