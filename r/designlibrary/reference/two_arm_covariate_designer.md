# Create a simple two arm design with a possibly prognostic covariate

Builds a design with one treatment and one control arm. Treatment
effects can be specified either by providing `control_mean` and
`treatment_mean` or by specifying a `control_mean` and `ate`. Non random
assignment is specified by a possible correlation, `rho_WZ`, between `W`
and a latent variable that determines the probability of `Z`.
Nonignorability is specified by a possible correlation, `rho_WY`,
between `W` and outcome `Y`.

## Usage

``` r
two_arm_covariate_designer(
  N = 100,
  prob = 0.5,
  control_mean = 0,
  sd = 1,
  ate = 1,
  h = 0,
  treatment_mean = control_mean + ate,
  rho_WY = 0,
  rho_WZ = 0,
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Sample size.

- prob:

  A number in \[0,1\]. Probability of assignment to treatment.

- control_mean:

  A number. Average outcome in control.

- sd:

  A positive number. Standard deviation of shock on Y.

- ate:

  A number. Average treatment effect.

- h:

  A number. Controls heterogeneous treatment effects by W. Defaults to
  0.

- treatment_mean:

  A number. Average outcome in treatment. Overrides `ate` if both
  specified.

- rho_WY:

  A number in \[-1,1\]. Correlation between W and Y.

- rho_WZ:

  A number in \[-1,1\]. Correlation between W and Z.

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A simple two-arm design with covariate W.

## Details

Units are assigned to treatment using complete random assignment.
Potential outcomes are normally distributed according to the mean and sd
arguments.

See [vignette
online](https://declaredesign.org/r/designlibrary/articles/two_arm.html).

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
#Generate a simple two-arm design using default arguments
two_arm_covariate_design <- two_arm_covariate_designer()
# Design with no confounding but a prognostic covariate 
prognostic <- two_arm_covariate_designer(N = 40, ate = .2, rho_WY = .9, h = .5)
if (FALSE) { # \dontrun{
diagnose_design(prognostic)
} # }
# Design with confounding 
confounding <- two_arm_covariate_designer(N = 40, ate = 0, rho_WZ = .9, rho_WY = .9, h = .5)
if (FALSE) { # \dontrun{
diagnose_design(confounding, sims = 2000)
} # }

# Curse of power: A biased design may be more likely to mislead the larger it is 
curses <- expand_design(two_arm_covariate_designer, 
                        N = c(50, 500, 5000), ate = 0, rho_WZ = .2, rho_WY = .2)
if (FALSE) { # \dontrun{
diagnoses <- diagnose_design(curses)
subset(diagnoses$diagnosands_df, estimator == "No controls")[,c("N", "power")]
} # }
```
