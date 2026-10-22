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
#> 1     ATE      0.2 estimator    Z 0.1671411 0.08082312  2.067986 0.03994111
#>      conf.low conf.high  df outcome
#> 1 0.007756481 0.3265257 198       Y

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
#> # A tibble: 6 × 8
#>   villages N_households households N_members individuals   age gender     Z
#>   <chr>           <int> <chr>          <dbl> <chr>       <int>  <int> <int>
#> 1 01                 71 0001               2 0001           80      0     0
#> 2 01                 71 0001               2 0002           79      0     0
#> 3 01                 71 0002               2 0003           29      1     0
#> 4 01                 71 0002               2 0004           37      1     0
#> 5 01                 71 0003               2 0005           33      1     0
#> 6 01                 71 0003               2 0006           73      0     1

## Cluster random assignment
design <-
  model +
  declare_assignment(Z = cluster_ra(clusters = villages,
                                    n = 15))
                                    
head(draw_data(design))
#> # A tibble: 6 × 8
#>   villages N_households households N_members individuals   age gender Z    
#>   <chr>           <int> <chr>          <dbl> <chr>       <int>  <int> <fct>
#> 1 01                 92 0001               2 0001           74      0 T5   
#> 2 01                 92 0001               2 0002           28      0 T5   
#> 3 01                 92 0002               1 0003           27      1 T5   
#> 4 01                 92 0003               3 0004           44      0 T5   
#> 5 01                 92 0003               3 0005           25      1 T5   
#> 6 01                 92 0003               3 0006           71      1 T5   

## Block and cluster random assignment
design <-
  model +
  declare_assignment(Z  = block_and_cluster_ra(
    blocks = villages,
    clusters = households,
    block_m = rep(20, 30)
  ))

head(draw_data(design))
#> # A tibble: 6 × 8
#>   villages N_households households N_members individuals   age gender     Z
#>   <chr>           <int> <chr>          <dbl> <chr>       <int>  <int> <int>
#> 1 01                 67 0001               4 0001           42      1     0
#> 2 01                 67 0001               4 0002           67      1     0
#> 3 01                 67 0001               4 0003           54      1     0
#> 4 01                 67 0001               4 0004           21      0     0
#> 5 01                 67 0002               2 0005           90      0     0
#> 6 01                 67 0002               2 0006           89      0     0

## Block random assignment
design <-
  model +
  declare_assignment(Z = block_ra(blocks = gender, m = 100))
  
head(draw_data(design))
#> # A tibble: 6 × 8
#>   villages N_households households N_members individuals   age gender     Z
#>   <chr>           <int> <chr>          <dbl> <chr>       <int>  <int> <int>
#> 1 01                 61 0001               1 0001           43      1     0
#> 2 01                 61 0002               2 0002           45      0     0
#> 3 01                 61 0002               2 0003           75      1     0
#> 4 01                 61 0003               3 0004           31      0     0
#> 5 01                 61 0003               3 0005           20      1     0
#> 6 01                 61 0003               3 0006           18      1     0

## Block random assignment using probabilities
design <-
  model +
  declare_assignment(Z = block_ra(blocks = gender,
                                  block_prob = c(1 / 3, 2 / 3)))

head(draw_data(design))
#> # A tibble: 6 × 8
#>   villages N_households households N_members individuals   age gender     Z
#>   <chr>           <int> <chr>          <dbl> <chr>       <int>  <int> <int>
#> 1 01                 73 0001               3 0001           79      0     0
#> 2 01                 73 0001               3 0002           80      0     0
#> 3 01                 73 0001               3 0003           19      0     0
#> 4 01                 73 0002               4 0004           28      1     1
#> 5 01                 73 0002               4 0005           48      1     1
#> 6 01                 73 0002               4 0006           52      1     1

## Factorial assignment
design <-
  model +
  declare_assignment(Z1 = complete_ra(N = N, m = 100),
                     Z2 = block_ra(blocks = Z1))

head(draw_data(design))
#> # A tibble: 6 × 9
#>   villages N_households households N_members individuals   age gender    Z1
#>   <chr>           <int> <chr>          <dbl> <chr>       <int>  <int> <int>
#> 1 01                 54 0001               2 0001           32      1     0
#> 2 01                 54 0001               2 0002           89      1     0
#> 3 01                 54 0002               3 0003           90      0     0
#> 4 01                 54 0002               3 0004           65      0     0
#> 5 01                 54 0002               3 0005           60      0     0
#> 6 01                 54 0003               4 0006           42      1     0
#> # ℹ 1 more variable: Z2 <int>

## Assignment using functions outside of randomizr
design <-
  model +
  declare_assignment(Z = rbinom(n = N, size = 1, prob = 0.35))

head(draw_data(design))
#> # A tibble: 6 × 8
#>   villages N_households households N_members individuals   age gender     Z
#>   <chr>           <int> <chr>          <dbl> <chr>       <int>  <int> <int>
#> 1 01                 81 0001               4 0001           67      0     1
#> 2 01                 81 0001               4 0002           86      0     0
#> 3 01                 81 0001               4 0003           49      0     0
#> 4 01                 81 0001               4 0004           51      0     0
#> 5 01                 81 0002               4 0005           22      1     0
#> 6 01                 81 0002               4 0006           53      1     0
```
