# Declare inquiry

Declares inquiries, or the inferential target of interest. Conceptually
very close to "estimand" or "quantity of interest".

## Usage

``` r
declare_inquiry(..., handler = inquiry_handler, label = "inquiry")

declare_inquiries(..., handler = inquiry_handler, label = "inquiry")

declare_estimand(...)

declare_estimands(...)

inquiry_handler(data, ..., subset = NULL, term = FALSE, label)
```

## Arguments

- ...:

  arguments to be captured, and later passed to the handler

- handler:

  a tidy-in, tidy-out function

- label:

  a string describing the step

- data:

  a data.frame

- subset:

  a subset expression

- term:

  TRUE/FALSE

## Value

a function, I(), that accepts a data.frame as an argument and returns a
data.frame containing the value of the inquiry, a^m.

## Details

For the default diagnosands, the return value of the handler should have
`inquiry` and `estimand` columns.

If term is TRUE, the names of ... will be returned in a `term` column,
and `inquiry` will contain the step label. This can be used as an
additional dimension for use in diagnosis.

## Examples

``` r

# Set up a design for use in examples:
## Two-arm randomized experiment
design <-
  declare_model(
    N = 500,
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_assignment(Z = complete_ra(N = N, m = 250)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z))
  
head(draw_data(design))
#>    ID X           U       Y_Z_0      Y_Z_1 Z           Y
#> 1 001 0  0.45042799  0.45042799  0.6504280 0  0.45042799
#> 2 002 0 -0.05733307 -0.05733307  0.1426669 0 -0.05733307
#> 3 003 0  0.22940944  0.22940944  0.4294094 0  0.22940944
#> 4 004 0 -0.01188688 -0.01188688  0.1881131 0 -0.01188688
#> 5 005 0 -0.54085735 -0.54085735 -0.3408573 0 -0.54085735
#> 6 006 0  0.51263288  0.51263288  0.7126329 1  0.71263288

# Some common inquiries
design +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
#> 
#> Research design declaration summary
#> 
#> Step 1 (model): declare_model(N = 500, X = rep(c(0, 1), each = N/2), U = rnorm(N, sd = 0.25), potential_outcomes(Y ~ 0.2 * Z + X + U)) 
#> 
#> Step 2 (assignment): declare_assignment(Z = complete_ra(N = N, m = 250)) -------
#> 
#> Step 3 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ Z)) ----------
#> 
#> Step 4 (inquiry): declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) -------------------
#> 
#> Run of the design:
#> 
#>  inquiry estimand
#>      ATE      0.2
#> 

design +
  declare_inquiry(difference_in_var = var(Y_Z_1) - var(Y_Z_0))
#> 
#> Research design declaration summary
#> 
#> Step 1 (model): declare_model(N = 500, X = rep(c(0, 1), each = N/2), U = rnorm(N, sd = 0.25), potential_outcomes(Y ~ 0.2 * Z + X + U)) 
#> 
#> Step 2 (assignment): declare_assignment(Z = complete_ra(N = N, m = 250)) -------
#> 
#> Step 3 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ Z)) ----------
#> 
#> Step 4 (inquiry): declare_inquiry(difference_in_var = var(Y_Z_1) - var(Y_Z_0)) -
#> 
#> Run of the design:
#> 
#>            inquiry  estimand
#>  difference_in_var -5.55e-17
#> 

design +
  declare_inquiry(mean_Y = mean(Y))
#> 
#> Research design declaration summary
#> 
#> Step 1 (model): declare_model(N = 500, X = rep(c(0, 1), each = N/2), U = rnorm(N, sd = 0.25), potential_outcomes(Y ~ 0.2 * Z + X + U)) 
#> 
#> Step 2 (assignment): declare_assignment(Z = complete_ra(N = N, m = 250)) -------
#> 
#> Step 3 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ Z)) ----------
#> 
#> Step 4 (inquiry): declare_inquiry(mean_Y = mean(Y)) ----------------------------
#> 
#> Run of the design:
#> 
#>  inquiry estimand
#>   mean_Y    0.575
#> 

# Inquiries among a subset
design +
  declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0),
                  subset = (Z == 1))
#> 
#> Research design declaration summary
#> 
#> Step 1 (model): declare_model(N = 500, X = rep(c(0, 1), each = N/2), U = rnorm(N, sd = 0.25), potential_outcomes(Y ~ 0.2 * Z + X + U)) 
#> 
#> Step 2 (assignment): declare_assignment(Z = complete_ra(N = N, m = 250)) -------
#> 
#> Step 3 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ Z)) ----------
#> 
#> Step 4 (inquiry): declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0), subset = (Z == 1)) 
#> 
#> Run of the design:
#> 
#>  inquiry estimand
#>      ATT      0.2
#> 

design +
  declare_inquiry(CATE = mean(Y_Z_1 - Y_Z_0),
                  subset = X == 1)
#> 
#> Research design declaration summary
#> 
#> Step 1 (model): declare_model(N = 500, X = rep(c(0, 1), each = N/2), U = rnorm(N, sd = 0.25), potential_outcomes(Y ~ 0.2 * Z + X + U)) 
#> 
#> Step 2 (assignment): declare_assignment(Z = complete_ra(N = N, m = 250)) -------
#> 
#> Step 3 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ Z)) ----------
#> 
#> Step 4 (inquiry): declare_inquiry(CATE = mean(Y_Z_1 - Y_Z_0), subset = X == 1) -
#> 
#> Run of the design:
#> 
#>  inquiry estimand
#>     CATE      0.2
#> 
                  
# equivalently
design +
  declare_inquiry(CATE = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]))
#> 
#> Research design declaration summary
#> 
#> Step 1 (model): declare_model(N = 500, X = rep(c(0, 1), each = N/2), U = rnorm(N, sd = 0.25), potential_outcomes(Y ~ 0.2 * Z + X + U)) 
#> 
#> Step 2 (assignment): declare_assignment(Z = complete_ra(N = N, m = 250)) -------
#> 
#> Step 3 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ Z)) ----------
#> 
#> Step 4 (inquiry): declare_inquiry(CATE = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1])) --
#> 
#> Run of the design:
#> 
#>  inquiry estimand
#>     CATE      0.2
#> 

# Add inquiries to a design along with estimators that
# reference them
diff_in_variances <-
  function(data) {
    data.frame(estimate = with(data, var(Y[Z == 1]) - var(Y[Z == 0])))
  }

design_1 <-
  design +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0),
                  difference_in_var = var(Y_Z_1) - var(Y_Z_0)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, 
                    inquiry = "ATE",
                    label = "DIM") +
  declare_estimator(handler =
                      label_estimator(diff_in_variances),
                    inquiry = "difference_in_var",
                    label = "DIV")

run_design(design_1)
#>             inquiry estimand estimator term   estimate  std.error statistic
#> 1               ATE      0.2       DIM    Z 0.13674750 0.04807476  2.844476
#> 2 difference_in_var      0.0       DIV <NA> 0.01854473         NA        NA
#>       p.value   conf.low conf.high  df outcome
#> 1 0.004631529 0.04229315 0.2312019 498       Y
#> 2          NA         NA        NA  NA    <NA>

# Two inquiries using one estimator

design_2 <-
  design +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0), subset = (Z == 1)) +
  declare_estimator(Y ~ Z, inquiry = c("ATE", "ATT"))

run_design(design_2)
#>   inquiry estimand estimator term  estimate  std.error statistic      p.value
#> 1     ATE      0.2 estimator    Z 0.2897546 0.05087063  5.695911 2.100901e-08
#> 2     ATT      0.2 estimator    Z 0.2897546 0.05087063  5.695911 2.100901e-08
#>    conf.low conf.high  df outcome
#> 1 0.1898071 0.3897021 498       Y
#> 2 0.1898071 0.3897021 498       Y

# Two inquiries using different coefficients from one estimator

design_3 <-
  design +
  declare_inquiry(intercept = mean(Y_Z_0),
                  slope = mean(Y_Z_1 - Y_Z_0)) +
  declare_estimator(
    Y ~ Z,
    .method = lm_robust,
    term = TRUE,
    inquiry = c("intercept", "slope")
  )

run_design(design_3)
#>     inquiry  estimand estimator        term  estimate  std.error statistic
#> 1 intercept 0.5098828 estimator (Intercept) 0.5187536 0.03584575 14.471830
#> 2     slope 0.2000000 estimator           Z 0.1822586 0.05012282  3.636239
#>        p.value   conf.low conf.high  df outcome
#> 1 7.140318e-40 0.44832602 0.5891811 498       Y
#> 2 3.054480e-04 0.08378031 0.2807368 498       Y


# declare_inquiries usage
design_4 <- design +
  declare_inquiries(
    ATE = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]),
    CATE_X0 = mean(Y_Z_1[X == 0] - Y_Z_0[X == 0]),
    CATE_X1 = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]),
    Difference_in_CATEs = CATE_X1 - CATE_X0,
    mean_Y = mean(Y))
    
run_design(design_4)
#>               inquiry      estimand
#> 1                 ATE  2.000000e-01
#> 2             CATE_X0  2.000000e-01
#> 3             CATE_X1  2.000000e-01
#> 4 Difference_in_CATEs -5.551115e-17
#> 5              mean_Y  6.085101e-01

```
