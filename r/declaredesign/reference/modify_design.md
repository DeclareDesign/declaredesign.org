# Modify a design after the fact

Insert, delete and replace steps in an (already declared) design object.

## Usage

``` r
insert_step(design, new_step, before, after)

delete_step(design, step)

replace_step(design, step, new_step)
```

## Arguments

- design:

  A design object, usually created using the + operator,
  [`expand_design`](https://declaredesign.org/r/declaredesign/reference/expand_design.md),
  or the design library.

- new_step:

  The new step; Either a function or a partial call.

- before:

  The step before which to add steps.

- after:

  The step after which to add steps.

- step:

  The quoted label of the step to be deleted or replaced.

## Value

A new design object.

## Details

See `modify_design` for details.

## Examples

``` r

 my_model <- 
   declare_model(
     N = 100, 
     U = rnorm(N),
     Y_Z_0 = U,
     Y_Z_1 = U + rnorm(N, mean = 2, sd = 2)
   )

 my_assignment <- declare_assignment(Z = complete_ra(N, m = 50))
 my_assignment_2 <- declare_assignment(Z = complete_ra(N, m = 25))

 design <- my_model + my_assignment

 draw_data(design)
#> # A tibble: 100 × 5
#>    ID         U  Y_Z_0  Y_Z_1     Z
#>    <chr>  <dbl>  <dbl>  <dbl> <int>
#>  1 001   -1.12  -1.12   1.29      0
#>  2 002   -1.14  -1.14  -0.321     1
#>  3 003    0.993  0.993  0.381     0
#>  4 004   -0.732 -0.732  0.758     0
#>  5 005    2.17   2.17   1.57      0
#>  6 006   -1.29  -1.29  -1.40      1
#>  7 007   -0.707 -0.707 -0.664     0
#>  8 008   -0.536 -0.536  5.59      1
#>  9 009   -1.08  -1.08   2.75      1
#> 10 010    0.352  0.352  2.90      1
#> # ℹ 90 more rows
 
 design_modified <- replace_step(design, 2, my_assignment_2)
 
 draw_data(design)
#> # A tibble: 100 × 5
#>    ID         U  Y_Z_0  Y_Z_1     Z
#>    <chr>  <dbl>  <dbl>  <dbl> <int>
#>  1 001    0.148  0.148  7.42      0
#>  2 002    0.168  0.168  1.08      1
#>  3 003   -1.95  -1.95   0.481     1
#>  4 004   -0.487 -0.487  2.98      0
#>  5 005    0.888  0.888  5.98      1
#>  6 006    0.444  0.444  2.33      0
#>  7 007    0.173  0.173 -0.942     0
#>  8 008    0.977  0.977  2.51      1
#>  9 009    0.389  0.389  1.34      1
#> 10 010    0.163  0.163  2.16      1
#> # ℹ 90 more rows
 
 if (FALSE) { # \dontrun{
 
 design <- 
   declare_model(
     N = 100,
     U = rnorm(N),
     potential_outcomes(Y ~ 0.20 * Z + U)
   ) + 
     declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
     declare_assignment(Z = complete_ra(N, m = N/2)) + 
     declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
     declare_estimator(Y ~ Z, inquiry = "ATE")
 
 insert_step(design, declare_sampling(S = complete_rs(N, n = 50)),
             after = 1)

 # If you are using a design created by a designer, for example from
 # the DesignLibrary package, you will not have access to the step
 # objects. Instead, you can always use the label of the step.
 
 design <- DesignLibrary::two_arm_designer()
 
 # get the labels for the steps
 names(design)
 
 insert_step(design, 
   declare_sampling(S = complete_rs(N, n = 50)),
   after = "potential_outcomes")
   
 } # }


design <- 
  declare_model(
    N = 100,
    U = rnorm(N),
    potential_outcomes(Y ~ 0.20 * Z + U)
  ) + 
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
    declare_assignment(Z = complete_ra(N, m = N/2)) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
    declare_estimator(Y ~ Z, inquiry = "ATE")
delete_step(design, step = 5)
#> 
#> Research design declaration summary
#> 
#> Step 1 (model): declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ 0.2 * Z + U)) 
#> 
#> Step 2 (inquiry): declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) -------------------
#> 
#> Step 3 (assignment): declare_assignment(Z = complete_ra(N, m = N/2)) -----------
#> 
#> Step 4 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ Z)) ----------
#> 
#> Run of the design:
#> 
#>  inquiry estimand
#>      ATE      0.2
#> 
#> No modifiable parameters saved in design 


design <- 
  declare_model(
    N = 100,
    U = rnorm(N),
    potential_outcomes(Y ~ 0.20 * Z + U)
  ) + 
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
    declare_assignment(Z = complete_ra(N, m = N/2)) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
    declare_estimator(Y ~ Z, inquiry = "ATE")

replace_step(
  design, 
  step = 3, 
  new_step = declare_assignment(Z = simple_ra(N, prob = 0.5)))
#> 
#> Research design declaration summary
#> 
#> Step 1 (model): declare_model(N = 100, U = rnorm(N), potential_outcomes(Y ~ 0.2 * Z + U)) 
#> 
#> Step 2 (inquiry): declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) -------------------
#> 
#> Step 3 (assignment): declare_assignment(Z = simple_ra(N, prob = 0.5)) ----------
#> 
#> Step 4 (measurement): declare_measurement(Y = reveal_outcomes(Y ~ Z)) ----------
#> 
#> Step 5 (estimator): declare_estimator(Y ~ Z, inquiry = "ATE") ------------------
#> 
#> Run of the design:
#> 
#>  inquiry estimand estimator term estimate std.error statistic p.value conf.low
#>      ATE      0.2 estimator    Z    0.115     0.217     0.527     0.6   -0.317
#>  conf.high df outcome
#>      0.546 98       Y
#> 
#> No modifiable parameters saved in design 
```
