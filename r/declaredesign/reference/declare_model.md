# Declare the size and features of the population.

The model component of a research design captures background beliefs
about the world in which a study is implemented, the variables of
interest, and how these respond to each other. The `declare_model`
function writes functions that themselves create databases that
represent these features of possible worlds.

## Usage

``` r
declare_model(..., handler = fabricate, label = NULL)
```

## Arguments

- ...:

  arguments to be captured, and later passed to the handler

- handler:

  a tidy-in, tidy-out function

- label:

  a string describing the step

## Value

A function that returns a data.frame.

## Examples

``` r
# declare_model is usually used when concatenating
# design elements with `+`

## Example: Two-arm randomized experiment
design <-
  declare_model(
    N = 500,
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N = N, m = 250)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")

# declare_model returns a function:
M <- declare_model(N = 100)
M()
#> # A tibble: 100 × 1
#>    ID   
#>    <chr>
#>  1 001  
#>  2 002  
#>  3 003  
#>  4 004  
#>  5 005  
#>  6 006  
#>  7 007  
#>  8 008  
#>  9 009  
#> 10 010  
#> # ℹ 90 more rows

# You can also declare a population from existing data
M <- declare_model(data = mtcars)
M()
#> # A tibble: 32 × 11
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows

# You can resample from existing data
M <- declare_model(N = 100, data = mtcars, handler = resample_data)
M()
#> # A tibble: 100 × 11
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  15.8     8  351    264  4.22  3.17  14.5     0     1     5     4
#>  2  26       4  120.    91  4.43  2.14  16.7     0     1     5     2
#>  3  15.5     8  318    150  2.76  3.52  16.9     0     0     3     2
#>  4  16.4     8  276.   180  3.07  4.07  17.4     0     0     3     3
#>  5  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  6  10.4     8  460    215  3     5.42  17.8     0     0     3     4
#>  7  16.4     8  276.   180  3.07  4.07  17.4     0     0     3     3
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#> 10  13.3     8  350    245  3.73  3.84  15.4     0     0     3     4
#> # ℹ 90 more rows

# You can build up more variables using any predefined variables.
# For instance you can declare a model with observed covariates X1 
# and X2 and unobserved heterogeneity U that each affect outcome Y
M <- declare_model(
  N = 5,
  U = rnorm(N),
  X1 = rbinom(N, size = 1, prob = 0.5),
  X2 = X1 + rnorm(N),
  Y = 0.1 * X1 + 0.2 * X2 + 0.1 * X1 * X2 + U
)
M()
#> # A tibble: 5 × 5
#>   ID         U    X1     X2        Y
#>   <chr>  <dbl> <int>  <dbl>    <dbl>
#> 1 1     -0.201     1  1.10   0.228  
#> 2 2      0.788     0 -1.59   0.469  
#> 3 3      0.108     0 -0.526  0.00302
#> 4 4     -1.10      1  1.40  -0.578  
#> 5 5     -0.446     1  1.42   0.0813 

# You can build up a model over multiple design steps 
# Once steps are concatendated they become a design
M1 <- declare_model(
  N = 5,
  U = rnorm(N))
M2 <-  declare_model(
  X1 = rbinom(N, size = 1, prob = 0.5),
  X2 = X1 + rnorm(N))
M2(M1())
#> # A tibble: 5 × 4
#>   ID          U    X1      X2
#>   <chr>   <dbl> <int>   <dbl>
#> 1 1     -0.450      1  2.48  
#> 2 2      0.0140     0  0.630 
#> 3 3     -1.16       1  0.379 
#> 4 4      0.394      0  0.254 
#> 5 5     -0.148      0 -0.0418
design <- M1 + M2
draw_data(design)
#> # A tibble: 5 × 4
#>   ID         U    X1     X2
#>   <chr>  <dbl> <int>  <dbl>
#> 1 1      2.26      0  0.796
#> 2 2     -1.28      1  2.21 
#> 3 3     -0.155     1  1.82 
#> 4 4      0.296     0 -0.978
#> 5 5     -0.587     0 -1.28 

# More eleboate data types.
# We can draw correlated variables using draw_multivariate
M <-
declare_model(
  draw_multivariate(c(X1, X2) ~ MASS::mvrnorm(
    n = 1000,
    mu = c(0, 0),
    Sigma = matrix(c(1, 0.3, 0.3, 1), nrow = 2)
  )))
M()
#> # A tibble: 1,000 × 2
#>         X1      X2
#>      <dbl>   <dbl>
#>  1  0.624   0.693 
#>  2  0.360  -1.05  
#>  3  0.0311  0.0742
#>  4 -0.0620 -0.810 
#>  5  0.735  -0.545 
#>  6  0.478  -1.35  
#>  7  0.653   1.17  
#>  8  0.748   1.36  
#>  9  0.0850  1.13  
#> 10  0.325  -0.780 
#> # ℹ 990 more rows

# Declare potential outcomes model dependent on assignment Z
## Manually
M <- 
  declare_model(N = 100, 
                Y_Z_0 = rbinom(N, size = 1, prob = 0.5),
                Y_Z_1 = rbinom(N, size = 1, prob = 0.6)
  )
M()
#> # A tibble: 100 × 3
#>    ID    Y_Z_0 Y_Z_1
#>    <chr> <int> <int>
#>  1 001       1     0
#>  2 002       1     1
#>  3 003       1     1
#>  4 004       1     0
#>  5 005       1     0
#>  6 006       0     0
#>  7 007       1     1
#>  8 008       1     0
#>  9 009       0     0
#> 10 010       1     0
#> # ℹ 90 more rows

## Using the potential_outcomes function from fabricatr
M <- 
  declare_model(N = 100, 
                potential_outcomes(Y ~ rbinom(N, size = 1, prob = 0.1 * Z + 0.5))
  )
M()
#> # A tibble: 100 × 3
#>    ID    Y_Z_0 Y_Z_1
#>    <chr> <int> <int>
#>  1 001       1     1
#>  2 002       0     0
#>  3 003       1     1
#>  4 004       1     0
#>  5 005       0     0
#>  6 006       1     1
#>  7 007       1     1
#>  8 008       0     0
#>  9 009       0     0
#> 10 010       1     1
#> # ℹ 90 more rows
 
## we can draw from a distribution of effect sizes 
M <-
  declare_model(
    N = 100, 
    tau = runif(1, min = 0, max = 1), 
    U = rnorm(N), 
    potential_outcomes(Y ~ tau * Z + U)
  )
M()
#> # A tibble: 100 × 5
#>    ID      tau      U  Y_Z_0   Y_Z_1
#>    <chr> <dbl>  <dbl>  <dbl>   <dbl>
#>  1 001   0.844 -1.43  -1.43  -0.590 
#>  2 002   0.844 -0.500 -0.500  0.345 
#>  3 003   0.844  1.08   1.08   1.92  
#>  4 004   0.844  1.79   1.79   2.63  
#>  5 005   0.844 -1.77  -1.77  -0.923 
#>  6 006   0.844 -2.14  -2.14  -1.29  
#>  7 007   0.844 -0.173 -0.173  0.672 
#>  8 008   0.844  1.12   1.12   1.96  
#>  9 009   0.844 -0.910 -0.910 -0.0660
#> 10 010   0.844  0.586  0.586  1.43  
#> # ℹ 90 more rows

## we can simulate treatment-by-covariate effect heterogeneity:
M <- 
  declare_model(
    N = 100, 
    U = rnorm(N), 
    X = rbinom(N, 1, prob = 0.5),
    potential_outcomes(Y ~  0.3 * Z + 0.2*X + 0.1*Z*X + U)
  )
M()
#> # A tibble: 100 × 5
#>    ID         U     X  Y_Z_0   Y_Z_1
#>    <chr>  <dbl> <int>  <dbl>   <dbl>
#>  1 001    0.808     1  1.01   1.41  
#>  2 002    0.605     0  0.605  0.905 
#>  3 003   -1.29      1 -1.09  -0.687 
#>  4 004    0.596     0  0.596  0.896 
#>  5 005   -1.57      1 -1.37  -0.975 
#>  6 006   -0.688     1 -0.488 -0.0882
#>  7 007    1.87      0  1.87   2.17  
#>  8 008   -0.462     0 -0.462 -0.162 
#>  9 009   -1.05      1 -0.846 -0.446 
#> 10 010   -0.732     1 -0.532 -0.132 
#> # ℹ 90 more rows

## potential outcomes can respond to two treatments:
M <- declare_model(
  N = 6,
  U = rnorm(N),
  potential_outcomes(Y ~ Z1 + Z2 + U, 
                     conditions = list(Z1 = c(0, 1), Z2 = c(0, 1))))

M()
#> # A tibble: 6 × 6
#>   ID         U Y_Z1_0_Z2_0 Y_Z1_1_Z2_0 Y_Z1_0_Z2_1 Y_Z1_1_Z2_1
#>   <chr>  <dbl>       <dbl>       <dbl>       <dbl>       <dbl>
#> 1 1     -0.751      -0.751       0.249       0.249       1.25 
#> 2 2      0.304       0.304       1.30        1.30        2.30 
#> 3 3      0.125       0.125       1.13        1.13        2.13 
#> 4 4     -0.434      -0.434       0.566       0.566       1.57 
#> 5 5     -1.86       -1.86       -0.859      -0.859       0.141
#> 6 6     -0.465      -0.465       0.535       0.535       1.53 

# Declare a two-level hierarchical population
# containing varying numbers of individuals within
# households and an age variable defined at the individual
# level
M <- declare_model(
  households = add_level(
    N = 100, 
    N_members = sample(c(1, 2, 3, 4), N, 
                       prob = c(0.2, 0.3, 0.25, 0.25), 
                       replace = TRUE)
  ),
  individuals = add_level(
    N = N_members, 
    age = sample(18:90, N, replace = TRUE)
  )
)
M()
#> # A tibble: 266 × 4
#>    households N_members individuals   age
#>    <chr>          <dbl> <chr>       <int>
#>  1 001                1 001            76
#>  2 002                4 002            34
#>  3 002                4 003            53
#>  4 002                4 004            68
#>  5 002                4 005            35
#>  6 003                4 006            28
#>  7 003                4 007            85
#>  8 003                4 008            33
#>  9 003                4 009            53
#> 10 004                4 010            38
#> # ℹ 256 more rows

## Panel data have a more complex structure:
M <- 
  declare_model(
    countries = add_level(
      N = 196, 
      country_shock = rnorm(N)
    ),
    years = add_level(
      N = 100, 
      time_trend = 1:N,
      year_shock = runif(N, 1, 10), 
      nest = FALSE
    ),
    observation = cross_levels(
      by = join_using(countries, years),
      observation_shock = rnorm(N),
      Y = 0.01 * time_trend + country_shock + year_shock + observation_shock 
    )
  )
M()
#> Warning: `nest = FALSE` is deprecated in fabricatr 2.0.
#> Write:  declare_level(N = 100, time_trend = 1:N, year_shock = runif(N, 1, 10))
#> Not:    add_level(N = 100, time_trend = 1:N, year_shock = runif(N, 1, 10), nest = FALSE)
#> This warning is displayed once per session.
#> Warning: `by =` is deprecated in fabricatr 2.0.
#> Write:  cross_levels(.by = c("countries", "years"), observation_shock = rnorm(N), Y = 0.01 * time_trend + country_shock + year_shock + observation_shock)
#> Not:    cross_levels(by = join_using(countries, years), observation_shock = rnorm(N), Y = 0.01 * time_trend + country_shock + year_shock + observation_shock)
#> This warning is displayed once per session.
#> # A tibble: 19,600 × 8
#>    countries country_shock years time_trend year_shock observation
#>    <chr>             <dbl> <chr>      <int>      <dbl> <chr>      
#>  1 001              -1.78  001            1       5.21 00001      
#>  2 002              -0.651 001            1       5.21 00002      
#>  3 003              -0.702 001            1       5.21 00003      
#>  4 004               0.968 001            1       5.21 00004      
#>  5 005               0.152 001            1       5.21 00005      
#>  6 006              -1.19  001            1       5.21 00006      
#>  7 007               0.319 001            1       5.21 00007      
#>  8 008              -1.66  001            1       5.21 00008      
#>  9 009               0.689 001            1       5.21 00009      
#> 10 010              -0.200 001            1       5.21 00010      
#> # ℹ 19,590 more rows
#> # ℹ 2 more variables: observation_shock <dbl>, Y <dbl>

# Declare a population using a custom function
# the default handler is fabricatr::fabricate,
# but you can supply any function that returns a data.frame
my_model_function <- function(N) {
  data.frame(u = rnorm(N))
}

M <- declare_model(N = 10, handler = my_model_function)
M()
#>              u
#> 1   1.91250587
#> 2  -0.46366126
#> 3  -1.94005605
#> 4   0.30544451
#> 5  -0.95697605
#> 6  -0.04154724
#> 7  -0.75002531
#> 8  -0.66326961
#> 9  -1.25907144
#> 10  0.02430886
```
