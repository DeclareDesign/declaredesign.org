# Getting started

``` r

library(DeclareDesign)
#> Loading required package: randomizr
#> Loading required package: fabricatr
#> Loading required package: estimatr
```

`DeclareDesign` declares and diagnoses. Loading it also attaches three
companion packages that supply the pieces a declaration is written out
of: `fabricatr` makes data, `randomizr` randomizes, and `estimatr`
estimates.

## Declaration

Every design element is made with a function that starts with the word
`declare`. Here is one, an assignment that gives each unit a 60 percent
chance of treatment:

``` r

simple_random_assignment <- declare_assignment(Z = simple_ra(N = N, prob = 0.6))
```

A design element is a function of data. Hand it the same six units twice
and it assigns them differently each time, because it is the procedure
that is declared, not one fixed vector of assignments:

``` r

units <- tibble::tibble(ID = 1:6)

simple_random_assignment(units)
#> # A tibble: 6 × 2
#>      ID     Z
#>   <int> <int>
#> 1     1     0
#> 2     2     1
#> 3     3     1
#> 4     4     1
#> 5     5     1
#> 6     6     1

simple_random_assignment(units)
#> # A tibble: 6 × 2
#>      ID     Z
#>   <int> <int>
#> 1     1     0
#> 2     2     0
#> 3     3     0
#> 4     4     0
#> 5     5     0
#> 6     6     1
```

A two-arm randomized experiment with 100 units puts five elements
together with `+`. The sample size is given a name above the design,
because it is the number we intend to vary later; everything else is
written down as part of the design:

``` r

n_units <- 100

declaration <-
  declare_model(
    N = n_units,
    U = rnorm(N),
    potential_outcomes(Y ~ 0.2 * Z + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1) - mean(Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, .method = difference_in_means, inquiry = "ATE")
```

The model says what the world is like: 100 units, an unobserved `U`, and
two potential outcomes. `potential_outcomes(Y ~ 0.2 * Z + U)` evaluates
its formula once with `Z = 0` and once with `Z = 1`, so it writes two
columns, `Y_Z_0` and `Y_Z_1`; the `Z` that units actually receive is
assigned two lines later. The inquiry is the question asked of that
world, here the average treatment effect, and its value on any one draw
is the estimand. The assignment and measurement steps are the data
strategy, and the estimator is the answer strategy. `inquiry = "ATE"`
tells the estimator which inquiry it is trying to answer.

Run the design to see what one study would look like:

``` r

head(draw_data(declaration))
#> # A tibble: 6 × 6
#>   ID         U  Y_Z_0  Y_Z_1     Z      Y
#>   <chr>  <dbl>  <dbl>  <dbl> <int>  <dbl>
#> 1 001   -0.924 -0.924 -0.724     1 -0.724
#> 2 002   -1.19  -1.19  -0.988     0 -1.19 
#> 3 003   -1.10  -1.10  -0.898     1 -0.898
#> 4 004    1.10   1.10   1.30      1  1.30 
#> 5 005   -1.60  -1.60  -1.40      0 -1.60 
#> 6 006    0.272  0.272  0.472     0  0.272
```

Both potential outcomes are in the data because you are simulating the
world. A real study observes `Y` alone. Each unit’s two potential
outcomes share the same `U`, so they differ by exactly 0.2 and the
average treatment effect is 0.2 in every run.

``` r

draw_estimands(declaration)
#> # A tibble: 1 × 2
#>   inquiry estimand
#>   <chr>      <dbl>
#> 1 ATE          0.2

draw_estimates(declaration)
#> # A tibble: 1 × 12
#>   term  estimate std.error statistic p.value conf.low conf.high    df outcome
#>   <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <dbl> <chr>  
#> 1 Z        0.157     0.201     0.783   0.435   -0.241     0.556  97.0 Y      
#> # ℹ 3 more variables: estimator <chr>, inquiry <chr>, estimand <dbl>
```

The estimand is 0.2, and one study’s estimate can miss it by a long way.
How far it misses on average, and how often a study like this one finds
a significant effect, is what diagnosis answers.

## Diagnosis

Diagnosis is the process of simulating the design many times and
calculating summary statistics about the design that describe its
properties, which we call diagnosands.

``` r

diagnosis <- diagnose_design(declaration, sims = 500)

diagnosis
#> Research design diagnosis
#>   500 simulations
#>   bootstrap standard errors in parentheses (100 replicates)
#> 
#>       Design Inquiry Estimator Outcome Term N Sims Mean Estimand Mean Estimate
#>  declaration     ATE estimator       Y    Z    500          0.20          0.20
#>                                                           (0.00)        (0.01)
#>    Bias SD Estimate   RMSE  Power Coverage
#>    0.00        0.20   0.20   0.17     0.96
#>  (0.01)      (0.01) (0.01) (0.02)   (0.01)
```

Bias sits near zero, so the estimator recovers the estimand on average.
Power is the number to look at: with 100 units and an effect of 0.2,
only about one run in six ends with a significant result. Coverage near
0.95 says the confidence intervals are the width they claim to be.

Each diagnosand comes with a bootstrapped standard error, so you can
tell a real difference from simulation noise.
[`?default_diagnosands`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md)
says what each one means.

## Redesign

We redesign to learn how the diagnosands change as design features
change.
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
over a range of sample sizes produces a list of designs:

``` r

designs <- redesign(declaration, n_units = c(100, 200, 300, 400, 500))

diagnose_design(designs, sims = 500)
#> Research design diagnosis
#>   500 simulations
#>   bootstrap standard errors in parentheses (100 replicates)
#> 
#>         Design n_units Inquiry Estimator Outcome Term N Sims Mean Estimand
#>  n_units = 100     100     ATE estimator       Y    Z    500          0.20
#>                                                                     (0.00)
#>  n_units = 200     200     ATE estimator       Y    Z    500          0.20
#>                                                                     (0.00)
#>  n_units = 300     300     ATE estimator       Y    Z    500          0.20
#>                                                                     (0.00)
#>  n_units = 400     400     ATE estimator       Y    Z    500          0.20
#>                                                                     (0.00)
#>  n_units = 500     500     ATE estimator       Y    Z    500          0.20
#>                                                                     (0.00)
#>  Mean Estimate   Bias SD Estimate   RMSE  Power Coverage
#>           0.20   0.00        0.20   0.20   0.17     0.96
#>         (0.01) (0.01)      (0.01) (0.01) (0.02)   (0.01)
#>           0.20   0.00        0.14   0.14   0.28     0.94
#>         (0.01) (0.01)      (0.00) (0.00) (0.02)   (0.01)
#>           0.20  -0.00        0.12   0.12   0.41     0.94
#>         (0.01) (0.01)      (0.00) (0.00) (0.02)   (0.01)
#>           0.20  -0.00        0.10   0.10   0.52     0.95
#>         (0.00) (0.00)      (0.00) (0.00) (0.02)   (0.01)
#>           0.20   0.00        0.08   0.08   0.63     0.97
#>         (0.00) (0.00)      (0.00) (0.00) (0.02)   (0.01)
```

A design can be redesigned over any name its expressions read from
outside it, as `n_units` is read here. A number written into the design,
as `0.2` is above, is part of the design: to redesign over values of it,
give it a name first.
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md)
names a value inside the design instead, which earns its place when
several steps read it or when a column shares its name.

The first row is the design diagnosed above, and the rows below it show
power rising with the sample size while bias stays where it was.
Choosing a sample size is now an argument about a table rather than a
hunch, and none of it required collecting data.

## A design is code

The declaration above is the whole design: the model, the inquiry, the
data strategy, and the answer strategy, in one object you can run,
diagnose, change, and send to a collaborator. The script that built it
is the code, and
[`?DeclareDesign`](https://declaredesign.org/r/declaredesign/reference/DeclareDesign-package.md)
lists the verbs.

The book at <https://book.declaredesign.org> works through the same
steps at length, and its library of designs covers the common ones.
