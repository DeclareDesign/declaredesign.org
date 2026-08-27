# Create a design for mediation analysis

A mediation analysis design that examines the effect of treatment (Z) on
mediator (M) and the effect of mediator (M) on outcome (Y) (given Z=0)
as well as direct effect of treatment (Z) on outcome (Y) (given M=0).
Analysis is implemented using an interacted regression model. Note this
model is not guaranteed to be unbiased despite randomization of Z
because of possible violations of sequential ignorability.

## Usage

``` r
mediation_analysis_designer(
  N = 200,
  a = 1,
  b = 0.4,
  c = 0,
  d = 0.5,
  rho = 0,
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Size of sample.

- a:

  A number. Parameter governing effect of treatment (Z) on mediator (M).

- b:

  A number. Effect of mediator (M) on outcome (Y) when Z = 0.

- c:

  A number. Interaction between mediator (M) and (Z) for outcome (Y).

- d:

  A number. Direct effect of treatment (Z) on outcome (Y), when M = 0.

- rho:

  A number in \[-1,1\]. Correlation between mediator (M) and outcome (Y)
  error terms. Non zero correlation implies a violation of sequential
  ignorability.

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A mediation analysis design.

## Details

See [vignette
online](https://declaredesign.org/r/designlibrary/articles/mediation_analysis.html).

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
# Generate a mediation analysis design using default arguments:
mediation_1 <- mediation_analysis_designer()
draw_estimands(mediation_1)
#>               inquiry estimand
#> 1          FirstStage    0.365
#> 2          Indirect_0    0.400
#> 3          Indirect_1    0.400
#> 4 Controlled_Direct_0    0.500
#> 5 Controlled_Direct_1    0.500
#> 6    Natural_Direct_0    0.500
#> 7    Natural_Direct_1    0.500
if (FALSE) { # \dontrun{
diagnose_design(mediation_1, sims = 1000)
} # }

# A design with a violation of sequential ignorability and heterogeneous effects:
mediation_2 <- mediation_analysis_designer(a = 1, rho = .5, c = 1, d = .75)
draw_estimands(mediation_2)
#>               inquiry estimand
#> 1          FirstStage    0.335
#> 2          Indirect_0    0.400
#> 3          Indirect_1    1.400
#> 4 Controlled_Direct_0    0.750
#> 5 Controlled_Direct_1    1.750
#> 6    Natural_Direct_0    1.275
#> 7    Natural_Direct_1    1.610
if (FALSE) { # \dontrun{
diagnose_design(mediation_2, sims = 1000)
} # }
```
