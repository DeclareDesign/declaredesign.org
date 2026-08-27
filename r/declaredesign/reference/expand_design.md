# Declare a design via a designer

`expand_design` easily generates a set of design from a designer
function.

## Usage

``` r
expand_design(designer, ..., expand = TRUE, prefix = "design")
```

## Arguments

- designer:

  a function which yields a design

- ...:

  Options sent to the designer

- expand:

  boolean - if true, form the crossproduct of the ..., otherwise recycle
  them

- prefix:

  prefix for the names of the designs, i.e. if you create two designs
  they would be named prefix_1, prefix_2

## Value

if set of designs is size one, the design, otherwise a \`by\`-list of
designs. Designs are given a parameters attribute with the values of
parameters assigned by expand_design.

## Examples

``` r

if (FALSE) { # \dontrun{

# in conjunction with DesignLibrary

library(DesignLibrary)

designs <- expand_design(multi_arm_designer, outcome_means = list(c(3,2,4), c(1,4,1)))

diagnose_design(designs)

# with a custom designer function

designer <- function(N) {
  design <- 
    declare_model(
      N = N, 
      U = rnorm(N),
      potential_outcomes(Y ~ 0.20 * Z + U)
    ) + 
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
    declare_assignment(Z = complete_ra(N, m = N/2)) + 
    declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
    declare_estimator(Y ~ Z, inquiry = "ATE")
  return(design)
}

# returns list of eight designs
designs <- expand_design(designer, N = seq(30, 100, 10))

# diagnose a list of designs created by expand_design or redesign
diagnosis <- diagnose_design(designs, sims = 50)

# returns a single design
large_design <- expand_design(designer, N = 200)

diagnose_large_design <- diagnose_design(large_design, sims = 50)

} # }
```
