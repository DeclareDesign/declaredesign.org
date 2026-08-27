# Create a binary instrumental variables design

Builds a design with one instrument, one binary explanatory variable,
and one outcome.

## Usage

``` r
binary_iv_designer(
  N = 100,
  type_probs = c(1/3, 1/3, 1/3, 0),
  assignment_probs = c(0.5, 0.5, 0.5, 0.5),
  a_Y = 1,
  b_Y = 0,
  d_Y = 0,
  outcome_sd = 1,
  a = c(1, 0, 0, 0) * a_Y,
  b = rep(b_Y, 4),
  d = rep(d_Y, 4),
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Sample size.

- type_probs:

  A vector of four numbers in \[0,1\]. Probability of each complier type
  (always-taker, never-taker, complier, defier).

- assignment_probs:

  A vector of four numbers in \[0,1\]. Probability of assignment to
  encouragement (Z) for each complier type (always-taker, never-taker,
  complier, defier). Under random assignment these are normally
  identical since complier status is not known to researchers in
  advance.

- a_Y:

  A real number. Constant in Y equation. Assumed constant across types.
  Overridden by `a` if specified.

- b_Y:

  A real number. Effect of X on Y equation. Assumed constant across
  types. Overridden by `b` if specified.

- d_Y:

  A real number. Effect of Z on Y. Assumed constant across types.
  Overridden by `d` if specified.

- outcome_sd:

  A real number. The standard deviation of the outcome.

- a:

  A vector of four numbers. Constant in Y equation for each complier
  type (always-taker, never-taker, complier, defier).

- b:

  A vector of four numbers. Slope on X in Y equation for each complier
  type (always-taker, never-taker, complier, defier).

- d:

  A vector of four numbers. Slope on Z in Y equation for each complier
  type (non zero implies violation of exclusion restriction).

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A simple instrumental variables design with binary instrument,
treatment, and outcome variables.

## Details

A researcher is interested in the effect of binary X on outcome Y. The
relationship is confounded because units that are more likely to be
assigned to X=1 have higher Y outcomes. A potential instrument Z is
examined, which plausibly causes X. The instrument can be used to assess
the effect of X on Y for units whose value of X depends on Z if Z does
not negatively affect X for some cases, affects X positively for some,
and affects Y only through X.

See [vignette
online](https://declaredesign.org/r/designlibrary/articles/binary_iv.html)
for more details on estimands.

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
# Generate a simple iv design: iv identifies late not ate 
binary_iv_design_1 <- binary_iv_designer(N = 1000, b = c(.1, .2, .3, .4))
if (FALSE) { # \dontrun{
diagnose_design(binary_iv_design_1)
} # }

# Generates a simple iv design with violation of monotonicity
binary_iv_design_2 <- binary_iv_designer(type_probs = c(.1,.1,.6, .2), b_Y = .5)
if (FALSE) { # \dontrun{
diagnose_design(binary_iv_design_2)
} # }

# Generates a simple iv design with violation of exclusion restriction
binary_iv_design_3 <- binary_iv_designer(d_Y = .5, b_Y = .5)
if (FALSE) { # \dontrun{
diagnose_design(binary_iv_design_3)
} # }

# Generates a simple iv design with violation of randomization
binary_iv_design_4 <- binary_iv_designer(N = 1000, assignment_probs = c(.2, .3, .7, .5), b_Y = .5)
if (FALSE) { # \dontrun{
diagnose_design(binary_iv_design_4)
} # }

# Generates a simple iv design with violation of first stage
binary_iv_design_5 <- binary_iv_designer(type_probs = c(.5,.5, 0, 0), b_Y = .5)
if (FALSE) { # \dontrun{
diagnose_design(binary_iv_design_5)
} # }
```
