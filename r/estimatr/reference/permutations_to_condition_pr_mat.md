# Builds condition probability matrices for Horvitz-Thompson estimation from permutation matrix

Builds condition probability matrices for Horvitz-Thompson estimation
from permutation matrix

## Usage

``` r
permutations_to_condition_pr_mat(permutations)
```

## Arguments

- permutations:

  A matrix where the rows are units and the columns are different
  treatment permutations; treated units must be represented with a 1 and
  control units with a 0

## Value

a numeric 2n\*2n matrix of marginal and joint condition treatment
probabilities to be passed to the `condition_pr_mat` argument of
[`horvitz_thompson`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md).

## Details

This function takes a matrix of permutations, for example from the
[`obtain_permutation_matrix`](https://declaredesign.org/r/randomizr/reference/obtain_permutation_matrix.html)
function in randomizr or through simulation and returns a 2n\*2n matrix
that can be used to fully specify the design for
[`horvitz_thompson`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md)
estimation. You can read more about these matrices in the documentation
for the
[`declaration_to_condition_pr_mat`](https://declaredesign.org/r/estimatr/reference/declaration_to_condition_pr_mat.md)
function.

This is done by passing this matrix to the `condition_pr_mat` argument
of

## See also

[`declare_ra`](https://declaredesign.org/r/randomizr/reference/declare_ra.html),
[`declaration_to_condition_pr_mat`](https://declaredesign.org/r/estimatr/reference/declaration_to_condition_pr_mat.md)

## Examples

``` r
# Complete randomization
perms <- replicate(1000, sample(rep(0:1, each = 50)))
comp_pr_mat <- permutations_to_condition_pr_mat(perms)

# Arbitrary randomization
possible_treats <- cbind(
  c(1, 1, 0, 1, 0, 0, 0, 1, 1, 0),
  c(0, 1, 1, 0, 1, 1, 0, 1, 0, 1),
  c(1, 0, 1, 1, 1, 1, 1, 0, 0, 0)
)
arb_pr_mat <- permutations_to_condition_pr_mat(possible_treats)
# Simulating a column to be realized treatment
z <- possible_treats[, sample(ncol(possible_treats), size = 1)]
y <- rnorm(nrow(possible_treats))
horvitz_thompson(y ~ z, condition_pr_mat = arb_pr_mat)
#>    Estimate Std. Error   t value  Pr(>|t|) CI Lower CI Upper DF
#> z 0.2434772  0.6767714 0.3597629 0.7190245 -1.08297 1.569925 NA
```
