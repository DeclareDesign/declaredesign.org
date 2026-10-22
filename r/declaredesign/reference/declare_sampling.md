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
#>   inquiry estimand estimator term  estimate  std.error statistic   p.value
#> 1     ATE      0.2 estimator    Z 0.1215871 0.08132779  1.495025 0.1365001
#>      conf.low conf.high  df outcome
#> 1 -0.03879277 0.2819669 198       Y

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
#> # A tibble: 6 × 8
#>   villages N_households households N_members individuals   age gender     S
#>   <chr>           <int> <chr>          <dbl> <chr>       <int>  <int> <dbl>
#> 1 01                 54 0001               2 0001           64      0     1
#> 2 01                 54 0005               3 0015           41      1     1
#> 3 01                 54 0005               3 0016           57      0     1
#> 4 01                 54 0005               3 0017           30      1     1
#> 5 01                 54 0006               4 0018           77      1     1
#> 6 01                 54 0009               3 0025           39      0     1

## Cluster random sampling
design <- model +
  declare_sampling(S = cluster_rs(clusters = villages, 
                                  n = 15))

head(draw_data(design))
#> # A tibble: 6 × 8
#>   villages N_households households N_members individuals   age gender     S
#>   <chr>           <int> <chr>          <dbl> <chr>       <int>  <int> <dbl>
#> 1 04                 81 0217               2 0546           81      0     1
#> 2 04                 81 0217               2 0547           18      1     1
#> 3 04                 81 0218               3 0548           73      0     1
#> 4 04                 81 0218               3 0549           60      1     1
#> 5 04                 81 0218               3 0550           49      1     1
#> 6 04                 81 0219               4 0551           80      1     1

## Strata and cluster random sampling
design <- model +
  declare_sampling(S  = strata_and_cluster_rs(
    strata = villages,
    clusters = households,
    strata_n = rep(20, 30)))
    
head(draw_data(design))
#> # A tibble: 6 × 8
#>   villages N_households households N_members individuals   age gender     S
#>   <chr>           <int> <chr>          <dbl> <chr>       <int>  <int> <dbl>
#> 1 01                 85 0003               4 0008           59      1     1
#> 2 01                 85 0003               4 0009           79      0     1
#> 3 01                 85 0003               4 0010           79      0     1
#> 4 01                 85 0003               4 0011           80      0     1
#> 5 01                 85 0004               1 0012           66      1     1
#> 6 01                 85 0007               3 0020           76      1     1

## Stratified random sampling
design <- model +
  declare_sampling(S = strata_rs(strata = gender, n = 100))

head(draw_data(design))
#> # A tibble: 6 × 8
#>   villages N_households households N_members individuals   age gender     S
#>   <chr>           <int> <chr>          <dbl> <chr>       <int>  <int> <dbl>
#> 1 01                 77 0001               2 0002           37      0     1
#> 2 01                 77 0009               3 0021           22      0     1
#> 3 01                 77 0011               4 0029           40      0     1
#> 4 01                 77 0026               4 0066           58      0     1
#> 5 01                 77 0032               4 0083           81      0     1
#> 6 01                 77 0033               2 0085           38      1     1
```
