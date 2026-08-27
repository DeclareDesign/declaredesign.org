# Declare a reveal outcomes step

Deprecated. Please use the `reveal_outcomes` function within a
`declare_measurement` declaration.

## Usage

``` r
declare_reveal(..., handler = declare_reveal_handler, label = NULL)

declare_reveal_handler(
  data = NULL,
  outcome_variables = Y,
  assignment_variables = Z,
  attrition_variables = NULL,
  ...
)
```

## Arguments

- ...:

  arguments to be captured, and later passed to the handler

- handler:

  a tidy-in, tidy-out function

- label:

  a string describing the step

- data:

  A data.frame containing columns for assignment and potential outcomes.

- outcome_variables:

  The outcome prefix(es) of the potential outcomes.

- assignment_variables:

  Unquoted name(s) of the assignment variable(s).

- attrition_variables:

  Unquoted name of the attrition variable.

## Details

Potential outcomes declarations indicate what outcomes would obtain for
different possible values of assignment variables. But realized outcomes
need to be "revealed." `declare_reveal` generates these realized
outcomes using information on potential outcomes (for instance generated
via `declare_potential_outcomes`) and the relevant assignment variables
(for example created by `declare_assignment`). Revelation steps are
usefully included after declaration of all assignments of conditions
required to determine the realized outcome. If a revelation is not
declared, DeclareDesign will try to guess appropriate revelations.
Explicit revelation is recommended however.

`declare_reveal` declares how outcomes should be realized. A
"revelation" uses the random assignment to pluck out the correct
potential outcomes (Gerber and Green 2012, Chapter 2). Revelation
requires that every named outcome variable is a function of every named
assignment variable within a step. Thus if multiple outcome variables
depend on different assignment variables, multiple revelations are
needed.

## Examples

``` r

design <- 
  declare_model(
    N = 100, 
    U = rnorm(N), 
    Y_Z_0 = U, 
    Y_Z_1 = U + rnorm(N, mean = 2, sd = 2)
  ) + 
  declare_assignment(Z = complete_ra(N, m = 50)) + 
  declare_measurement(Y = reveal_outcomes(Y ~ Z))
  
head(draw_data(design))
#>    ID          U      Y_Z_0      Y_Z_1 Z          Y
#> 1 001  0.6968270  0.6968270  4.0461701 1  4.0461701
#> 2 002 -0.5397195 -0.5397195 -0.1309124 1 -0.1309124
#> 3 003 -0.4720495 -0.4720495  3.3956502 1  3.3956502
#> 4 004  1.1412234  1.1412234  2.2531206 1  2.2531206
#> 5 005 -0.4690623 -0.4690623 -2.5358282 0 -0.4690623
#> 6 006 -0.5622351 -0.5622351  2.2793598 0 -0.5622351

# Declaring multiple assignment variables or multiple outcome variables

design   <- 
  declare_model(
    N = 10,
    potential_outcomes(Y1 ~ Z),
    potential_outcomes(Y2 ~ 1 + 2 * Z),
    potential_outcomes(Y3 ~ 1 - X * Z, conditions = list(X = 0:1, Z = 0:1))
  ) + 
  declare_assignment(Z = complete_ra(N)) + 
  declare_assignment(X = complete_ra(N)) + 
  declare_measurement(Y1 = reveal_outcomes(Y1 ~ Z), 
                      Y2 = reveal_outcomes(Y2 ~ Z),
                      Y3 = reveal_outcomes(Y3 ~ X + Z))
                      
head(draw_data(design))
#>   ID Y1_Z_0 Y1_Z_1 Y2_Z_0 Y2_Z_1 Y3_X_0_Z_0 Y3_X_1_Z_0 Y3_X_0_Z_1 Y3_X_1_Z_1 Z
#> 1 01      0      1      1      3          1          1          1          0 1
#> 2 02      0      1      1      3          1          1          1          0 0
#> 3 03      0      1      1      3          1          1          1          0 0
#> 4 04      0      1      1      3          1          1          1          0 0
#> 5 05      0      1      1      3          1          1          1          0 1
#> 6 06      0      1      1      3          1          1          1          0 0
#>   X Y1 Y2 Y3
#> 1 0  1  3  1
#> 2 1  0  1  1
#> 3 0  0  1  1
#> 4 0  0  1  1
#> 5 1  1  3  0
#> 6 1  0  1  1

design <- 
  declare_model(
    N = 100, 
    age = sample(18:95, N, replace = TRUE),
    potential_outcomes(Y ~ .25 * Z + .01 * age * Z),
    potential_outcomes(R ~ rbinom(n = N, size = 1, prob = pnorm(Y_Z_0)))
  ) + 
  declare_assignment(Z = complete_ra(N, m = 25))
  declare_measurement(R = reveal_outcomes(R ~ Z),
                      Y = reveal_outcomes(Y ~ Z),
                      Y = ifelse(R == 1, Y, NA))
#> declare_measurement(R = reveal_outcomes(R ~ Z), Y = reveal_outcomes(Y ~ 
#>     Z), Y = ifelse(R == 1, Y, NA))
                      
head(draw_data(design))
#>    ID age Y_Z_0 Y_Z_1 R_Z_0 R_Z_1 Z
#> 1 001  64     0  0.89     1     0 1
#> 2 002  29     0  0.54     1     0 0
#> 3 003  67     0  0.92     0     0 0
#> 4 004  55     0  0.80     1     0 1
#> 5 005  46     0  0.71     1     1 1
#> 6 006  18     0  0.43     0     0 1
```
