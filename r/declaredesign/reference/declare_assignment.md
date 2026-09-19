# Declare Data Strategy: Assignment

Add a step to a design that assigns units to different treatment
conditions. Assignment is part of the data strategy component of a
research design.

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
#>   inquiry estimand estimator term  estimate  std.error statistic    p.value
#> 1     ATE      0.2 estimator    Z 0.2017947 0.08095002   2.49283 0.01349305
#>     conf.low conf.high  df outcome
#> 1 0.04215982 0.3614295 198       Y

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
#> 1       01           61       0001         4        0001  80      1 0
#> 2       01           61       0001         4        0002  41      1 0
#> 3       01           61       0001         4        0003  21      0 0
#> 4       01           61       0001         4        0004  30      0 0
#> 5       01           61       0002         3        0005  47      1 0
#> 6       01           61       0002         3        0006  85      1 0

## Cluster random assignment
design <-
  model +
  declare_assignment(Z = cluster_ra(clusters = villages,
                                    n = 15))
                                    
head(draw_data(design))
#>   villages N_households households N_members individuals age gender   Z
#> 1       01           78       0001         3        0001  40      1 T14
#> 2       01           78       0001         3        0002  35      1 T14
#> 3       01           78       0001         3        0003  46      1 T14
#> 4       01           78       0002         4        0004  85      0 T14
#> 5       01           78       0002         4        0005  53      0 T14
#> 6       01           78       0002         4        0006  43      0 T14

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
#> 1       01           91       0001         3        0001  34      0 0
#> 2       01           91       0001         3        0002  90      1 0
#> 3       01           91       0001         3        0003  64      1 0
#> 4       01           91       0002         4        0004  89      1 1
#> 5       01           91       0002         4        0005  26      1 1
#> 6       01           91       0002         4        0006  58      0 1

## Block random assignment
design <-
  model +
  declare_assignment(Z = block_ra(blocks = gender, m = 100))
  
head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z
#> 1       01           85       0001         1        0001  68      0 0
#> 2       01           85       0002         4        0002  61      0 0
#> 3       01           85       0002         4        0003  70      0 0
#> 4       01           85       0002         4        0004  77      0 0
#> 5       01           85       0002         4        0005  19      0 0
#> 6       01           85       0003         4        0006  45      1 0

## Block random assignment using probabilities
design <-
  model +
  declare_assignment(Z = block_ra(blocks = gender,
                                  block_prob = c(1 / 3, 2 / 3)))

head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z
#> 1       01           85       0001         4        0001  37      0 0
#> 2       01           85       0001         4        0002  51      1 0
#> 3       01           85       0001         4        0003  47      0 0
#> 4       01           85       0001         4        0004  80      0 0
#> 5       01           85       0002         4        0005  40      0 1
#> 6       01           85       0002         4        0006  74      1 1

## Factorial assignment
design <-
  model +
  declare_assignment(Z1 = complete_ra(N = N, m = 100),
                     Z2 = block_ra(blocks = Z1))

head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z1 Z2
#> 1       01           84       0001         2        0001  76      0  0  0
#> 2       01           84       0001         2        0002  75      1  0  1
#> 3       01           84       0002         4        0003  50      0  0  0
#> 4       01           84       0002         4        0004  50      1  0  1
#> 5       01           84       0002         4        0005  41      0  0  1
#> 6       01           84       0002         4        0006  40      1  0  1

## Assignment using functions outside of randomizr
design <-
  model +
  declare_assignment(Z = rbinom(n = N, size = 1, prob = 0.35))

head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z
#> 1       01           52       0001         1        0001  87      0 0
#> 2       01           52       0002         4        0002  45      0 0
#> 3       01           52       0002         4        0003  64      1 1
#> 4       01           52       0002         4        0004  56      1 0
#> 5       01           52       0002         4        0005  20      0 0
#> 6       01           52       0003         2        0006  87      0 0
```
