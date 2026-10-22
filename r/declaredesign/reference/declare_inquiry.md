# Declare inquiry

Declares inquiries. A research design typically seeks to find answers to
questions. The questions are the inquiries and the correct answer to
questions (in fact, or under a model) are the estimands. The answers you
generate from data, which may or may not be correct, are the estimates.
`declare_inquiry` is used to make research questions explicit and to
calculate estimands. All inquiries should be answerable under the model.

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
#> # A tibble: 6 × 7
#>   ID        X      U  Y_Z_0 Y_Z_1     Z      Y
#>   <chr> <dbl>  <dbl>  <dbl> <dbl> <int>  <dbl>
#> 1 001       0 0.373  0.373  0.573     0 0.373 
#> 2 002       0 0.194  0.194  0.394     0 0.194 
#> 3 003       0 0.123  0.123  0.323     1 0.323 
#> 4 004       0 0.0422 0.0422 0.242     0 0.0422
#> 5 005       0 0.0465 0.0465 0.247     1 0.247 
#> 6 006       0 0.0887 0.0887 0.289     0 0.0887

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
#> No modifiable parameters saved in design 

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
#>  difference_in_var -1.11e-16
#> 
#> No modifiable parameters saved in design 

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
#>   mean_Y    0.574
#> 
#> No modifiable parameters saved in design 

# Inquiries among a subset of units
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
#> No modifiable parameters saved in design 

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
#> No modifiable parameters saved in design 
                  
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
#> No modifiable parameters saved in design 

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
#>             inquiry estimand estimator term    estimate  std.error statistic
#> 1               ATE      0.2       DIM    Z 0.256676309 0.05184455  4.950884
#> 2 difference_in_var      0.0       DIV <NA> 0.003802573         NA        NA
#>        p.value  conf.low conf.high  df outcome
#> 1 1.013028e-06 0.1548153 0.3585373 498       Y
#> 2           NA        NA        NA  NA    <NA>

# Two inquiries using one estimator

design_2 <-
  design +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_inquiry(ATT = mean(Y_Z_1 - Y_Z_0), subset = (Z == 1)) +
  declare_estimator(Y ~ Z, inquiry = c("ATE", "ATT"))

run_design(design_2)
#>   inquiry estimand estimator term estimate  std.error statistic     p.value
#> 1     ATE      0.2 estimator    Z 0.235006 0.04985585  4.713709 3.16158e-06
#> 2     ATT      0.2 estimator    Z 0.235006 0.04985585  4.713709 3.16158e-06
#>    conf.low conf.high  df outcome
#> 1 0.1370522 0.3329597 498       Y
#> 2 0.1370522 0.3329597 498       Y

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
#>     inquiry estimand estimator        term  estimate  std.error statistic
#> 1 intercept 0.503523 estimator (Intercept) 0.4713810 0.03539390 13.318141
#> 2     slope 0.200000 estimator           Z 0.2642841 0.04989076  5.297255
#>        p.value  conf.low conf.high  df outcome
#> 1 7.848288e-35 0.4018412 0.5409207 498       Y
#> 2 1.767927e-07 0.1662617 0.3623064 498       Y


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
#> 5              mean_Y  6.107573e-01

```
