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
#>   inquiry estimand estimator term   estimate  std.error statistic   p.value
#> 1     ATE      0.2 estimator    Z 0.09450127 0.07783236  1.214164 0.2261312
#>      conf.low conf.high  df outcome
#> 1 -0.05898551 0.2479881 198       Y

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
#> 1       01           83       0001         2        0001  66      0 0
#> 2       01           83       0001         2        0002  47      1 0
#> 3       01           83       0002         2        0003  36      1 1
#> 4       01           83       0002         2        0004  72      1 0
#> 5       01           83       0003         1        0005  64      1 0
#> 6       01           83       0004         1        0006  73      1 0

## Cluster random assignment
design <-
  model +
  declare_assignment(Z = cluster_ra(clusters = villages,
                                    n = 15))
                                    
head(draw_data(design))
#>   villages N_households households N_members individuals age gender  Z
#> 1       01           76       0001         1        0001  74      0 T7
#> 2       01           76       0002         4        0002  33      0 T7
#> 3       01           76       0002         4        0003  52      0 T7
#> 4       01           76       0002         4        0004  72      0 T7
#> 5       01           76       0002         4        0005  72      0 T7
#> 6       01           76       0003         4        0006  49      0 T7

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
#> 1       01           90       0001         4        0001  88      0 0
#> 2       01           90       0001         4        0002  61      0 0
#> 3       01           90       0001         4        0003  77      0 0
#> 4       01           90       0001         4        0004  26      0 0
#> 5       01           90       0002         4        0005  50      1 0
#> 6       01           90       0002         4        0006  56      0 0

## Block random assignment
design <-
  model +
  declare_assignment(Z = block_ra(blocks = gender, m = 100))
  
head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z
#> 1       01           54       0001         3        0001  67      0 0
#> 2       01           54       0001         3        0002  25      1 0
#> 3       01           54       0001         3        0003  74      1 0
#> 4       01           54       0002         1        0004  35      0 0
#> 5       01           54       0003         2        0005  55      1 0
#> 6       01           54       0003         2        0006  34      0 0

## Block random assignment using probabilities
design <-
  model +
  declare_assignment(Z = block_ra(blocks = gender,
                                  block_prob = c(1 / 3, 2 / 3)))

head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z
#> 1       01           73       0001         2        0001  40      0 0
#> 2       01           73       0001         2        0002  37      1 0
#> 3       01           73       0002         3        0003  28      0 1
#> 4       01           73       0002         3        0004  33      0 1
#> 5       01           73       0002         3        0005  63      0 0
#> 6       01           73       0003         4        0006  30      0 1

## Factorial assignment
design <-
  model +
  declare_assignment(Z1 = complete_ra(N = N, m = 100),
                     Z2 = block_ra(blocks = Z1))

head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z1 Z2
#> 1       01           56       0001         2        0001  55      1  0  1
#> 2       01           56       0001         2        0002  24      1  0  0
#> 3       01           56       0002         2        0003  83      1  0  0
#> 4       01           56       0002         2        0004  36      1  0  1
#> 5       01           56       0003         1        0005  43      1  0  0
#> 6       01           56       0004         4        0006  50      0  0  1

## Assignment using functions outside of randomizr
design <-
  model +
  declare_assignment(Z = rbinom(n = N, size = 1, prob = 0.35))

head(draw_data(design))
#>   villages N_households households N_members individuals age gender Z
#> 1       01           73       0001         3        0001  72      0 0
#> 2       01           73       0001         3        0002  22      0 0
#> 3       01           73       0001         3        0003  45      1 0
#> 4       01           73       0002         4        0004  72      1 1
#> 5       01           73       0002         4        0005  42      1 0
#> 6       01           73       0002         4        0006  27      1 0
```
