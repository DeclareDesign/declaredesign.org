# Explore your design

Explore your design

Print code to recreate a design

## Usage

``` r
print_code(design)

# S3 method for class 'design'
print(x, verbose = FALSE, ...)

# S3 method for class 'design'
summary(object, verbose = TRUE, ...)
```

## Arguments

- design:

  A design object, typically created using the + operator

- x:

  a design object, typically created using the + operator

- verbose:

  an indicator for printing a long summary of the design, defaults to
  `TRUE`

- ...:

  optional arguments to be sent to summary function

- object:

  a design object created using the + operator

## Examples

``` r

# Two-arm randomized experiment
design <-
  declare_model(
    N = 500,
    gender = rbinom(N, 1, 0.5),
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N = N, n = 200)) +
  declare_assignment(Z = complete_ra(N = N, m = 100)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")

# Use draw_data to create a dataset using a design
dat <- draw_data(design)

draw_data(design, data = dat, start = 2)
#> # A tibble: 200 × 9
#>    ID    gender     X         U     Y_Z_0   Y_Z_1     S     Z         Y
#>    <chr>  <int> <dbl>     <dbl>     <dbl>   <dbl> <dbl> <int>     <dbl>
#>  1 003        1     0  0.174     0.174     0.374      1     1  0.374   
#>  2 005        1     0 -0.337    -0.337    -0.137      1     1 -0.137   
#>  3 010        0     0  0.258     0.258     0.458      1     1  0.458   
#>  4 011        0     0 -0.328    -0.328    -0.128      1     0 -0.328   
#>  5 013        1     0 -0.0419   -0.0419    0.158      1     1  0.158   
#>  6 015        1     0  0.0495    0.0495    0.250      1     1  0.250   
#>  7 016        0     0 -0.106    -0.106     0.0938     1     0 -0.106   
#>  8 017        1     0 -0.535    -0.535    -0.335      1     0 -0.535   
#>  9 018        0     0 -0.000202 -0.000202  0.200      1     0 -0.000202
#> 10 020        0     0  0.145     0.145     0.345      1     1  0.345   
#> # ℹ 190 more rows

# Apply get_estimates
get_estimates(design, data = dat)
#>   estimator term  estimate  std.error statistic     p.value   conf.low
#> 1 estimator    Z 0.2269074 0.08128606  2.791467 0.005760772 0.06660986
#>   conf.high  df outcome inquiry
#> 1 0.3872049 198       Y     ATE


# Two-arm randomized experiment
design <-
  declare_model(
    N = 500,
    gender = rbinom(N, 1, 0.5),
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N = N, n = 200)) +
  declare_assignment(Z = complete_ra(N = N, m = 100)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")

print_code(design)
#> model <- declare_model(N = 500, gender = rbinom(N, 1, 0.5), X = rep(c(0, 1), each = N/2), U = rnorm(N, sd = 0.25), potential_outcomes(Y ~ 0.2 * Z + X + U)) 
#> 
#> ATE <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) 
#> 
#> sampling <- declare_sampling(S = complete_rs(N = N, n = 200)) 
#> 
#> assignment <- declare_assignment(Z = complete_ra(N = N, m = 100)) 
#> 
#> measurement <- declare_measurement(Y = reveal_outcomes(Y ~ Z)) 
#> 
#> estimator <- declare_estimator(Y ~ Z, inquiry = "ATE") 
#> 
#> my_design <- construct_design(steps = steps) 
#> 

summary(design)
#> 
#> Research design declaration summary
#> 
#> Step 1 (model): declare_model(N = 500, gender = rbinom(N, 1, 0.5), X = rep(c(0, 1), each = N/2), U = rnorm(N, sd = 0.25), potential_outcomes(Y ~ 0.2 * Z + X + U)) 
#> 
#> N = 500 
#> 
#> Added variable: ID 
#>  N_missing N_unique     class
#>          0      500 character
#> 
#> Added variable: gender 
#>     0    1
#>   233  267
#>  0.47 0.53
#> 
#> Added variable: X 
#>     0    1
#>   250  250
#>  0.50 0.50
#> 
#> Added variable: U 
#>    min median  mean  max   sd N_missing N_unique
#>  -0.83  -0.01 -0.01 0.91 0.25         0      500
#> 
#> Added variable: Y_Z_0 
#>    min median mean  max   sd N_missing N_unique
#>  -0.83   0.54 0.49 1.91 0.56         0      500
#> 
#> Added variable: Y_Z_1 
#>    min median mean  max   sd N_missing N_unique
#>  -0.63   0.74 0.69 2.11 0.56         0      500
#> 
#> Step 2 (inquiry): declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) -------------------
#> 
#> A single draw of the inquiry:
#>  inquiry estimand
#>      ATE      0.2
#> 
#> Step 3 (sampling): declare_sampling(S = complete_rs(N = N, n = 200)) -----------
#> 
#> N = 200 (300 subtracted) 
#> 
#> Added variable: S 
#>     1
#>   200
#>  1.00
#> 
#> Altered variable: ID 
#>   Before: 
#>  N_missing N_unique     class
#>          0      500 character
#> 
#>   After:
#>  N_missing N_unique     class
#>          0      200 character
#> 
#> Altered variable: gender 
#>   Before: 
#>     0    1
#>   233  267
#>  0.47 0.53
#> 
#>   After:
#>     0    1
#>    96  104
#>  0.48 0.52
#> 
#> Altered variable: X 
#>   Before: 
#>     0    1
#>   250  250
#>  0.50 0.50
#> 
#>   After:
#>     0    1
#>   103   97
#>  0.52 0.48
#> 
#> Altered variable: U 
#>   Before: 
#>    min median  mean  max   sd N_missing N_unique
#>  -0.83  -0.01 -0.01 0.91 0.25         0      500
#> 
#>   After:
#>    min median  mean  max   sd N_missing N_unique
#>  -0.58  -0.01 -0.01 0.59 0.23         0      200
#> 
#> Altered variable: Y_Z_0 
#>   Before: 
#>    min median mean  max   sd N_missing N_unique
#>  -0.83   0.54 0.49 1.91 0.56         0      500
#> 
#>   After:
#>   min median mean  max   sd N_missing N_unique
#>  -0.5   0.49 0.48 1.46 0.53         0      200
#> 
#> Altered variable: Y_Z_1 
#>   Before: 
#>    min median mean  max   sd N_missing N_unique
#>  -0.63   0.74 0.69 2.11 0.56         0      500
#> 
#>   After:
#>   min median mean  max   sd N_missing N_unique
#>  -0.3   0.69 0.68 1.66 0.53         0      200
#> 
#> Step 4 (assignment): declare_assignment(Z = complete_ra(N = N, m = 100)) -------
#> 
#> Added variable: Z 
#>     0    1
#>   100  100
#>  0.50 0.50
#> 
#> Step 5 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ Z)) ----------
#> 
#> Added variable: Y 
#>   min median mean  max   sd N_missing N_unique
#>  -0.4   0.54 0.58 1.66 0.54         0      200
#> 
#> Step 6 (estimator): declare_estimator(Y ~ Z, inquiry = "ATE") ------------------
#> 
#> Formula: Y ~ Z 
#> 
#> A single draw of the estimator:
#>  estimator term  estimate  std.error statistic    p.value    conf.low conf.high
#>  estimator    Z 0.1543567 0.07542213  2.046571 0.04202169 0.005622954 0.3030905
#>   df outcome inquiry
#>  198       Y     ATE
#> 


design <- 
  declare_model(
    N = 500, 
    noise = rnorm(N),
    Y_Z_0 = noise, 
    Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)
  ) + 
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_sampling(S = complete_rs(N, n = 250)) + 
  declare_assignment(Z = complete_ra(N, m = 25)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
  declare_estimator(Y ~ Z, inquiry = "ATE") 

summary(design)
#> 
#> Research design declaration summary
#> 
#> Step 1 (model): declare_model(N = 500, noise = rnorm(N), Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = 2, sd = 2)) 
#> 
#> N = 500 
#> 
#> Added variable: ID 
#>  N_missing N_unique     class
#>          0      500 character
#> 
#> Added variable: noise 
#>    min median mean  max   sd N_missing N_unique
#>  -2.77   0.09 0.09 2.63 0.97         0      500
#> 
#> Added variable: Y_Z_0 
#>    min median mean  max   sd N_missing N_unique
#>  -2.77   0.09 0.09 2.63 0.97         0      500
#> 
#> Added variable: Y_Z_1 
#>    min median mean  max   sd N_missing N_unique
#>  -5.19   2.13 2.08 9.03 2.24         0      500
#> 
#> Step 2 (inquiry): declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) -------------------
#> 
#> A single draw of the inquiry:
#>  inquiry estimand
#>      ATE 1.989141
#> 
#> Step 3 (sampling): declare_sampling(S = complete_rs(N, n = 250)) ---------------
#> 
#> N = 250 (250 subtracted) 
#> 
#> Added variable: S 
#>     1
#>   250
#>  1.00
#> 
#> Altered variable: ID 
#>   Before: 
#>  N_missing N_unique     class
#>          0      500 character
#> 
#>   After:
#>  N_missing N_unique     class
#>          0      250 character
#> 
#> Altered variable: noise 
#>   Before: 
#>    min median mean  max   sd N_missing N_unique
#>  -2.77   0.09 0.09 2.63 0.97         0      500
#> 
#>   After:
#>    min median mean  max   sd N_missing N_unique
#>  -2.77   0.15 0.09 2.63 0.99         0      250
#> 
#> Altered variable: Y_Z_0 
#>   Before: 
#>    min median mean  max   sd N_missing N_unique
#>  -2.77   0.09 0.09 2.63 0.97         0      500
#> 
#>   After:
#>    min median mean  max   sd N_missing N_unique
#>  -2.77   0.15 0.09 2.63 0.99         0      250
#> 
#> Altered variable: Y_Z_1 
#>   Before: 
#>    min median mean  max   sd N_missing N_unique
#>  -5.19   2.13 2.08 9.03 2.24         0      500
#> 
#>   After:
#>    min median mean  max   sd N_missing N_unique
#>  -3.16   1.95 2.07 9.03 2.25         0      250
#> 
#> Step 4 (assignment): declare_assignment(Z = complete_ra(N, m = 25)) ------------
#> 
#> Added variable: Z 
#>     0    1
#>   225   25
#>  0.90 0.10
#> 
#> Step 5 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ Z)) ----------
#> 
#> Added variable: Y 
#>    min median mean  max   sd N_missing N_unique
#>  -2.77   0.23 0.32 7.54 1.34         0      250
#> 
#> Step 6 (estimator): declare_estimator(Y ~ Z, inquiry = "ATE") ------------------
#> 
#> Formula: Y ~ Z 
#> 
#> A single draw of the estimator:
#>  estimator term estimate std.error statistic      p.value conf.low conf.high
#>  estimator    Z 2.374507 0.4036542  5.882528 1.301686e-08 1.579479  3.169534
#>   df outcome inquiry
#>  248       Y     ATE
#> 
```
