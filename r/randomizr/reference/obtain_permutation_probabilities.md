# Obtain the probabilities of permutations

Returns how likely each assignment in the permutation matrix was. Most
designs make every possible assignment equally likely, in which case
these are all the same and can be ignored. Blocked and clustered designs
of unequal size do not, and there the probabilities are needed to weight
the randomization distribution correctly.

## Usage

``` r
obtain_permutation_probabilities(declaration)
```

## Arguments

- declaration:

  A random assignment declaration, created by
  [`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md).
  (required)

## Value

A vector with one entry per possible assignment, giving the probability
that the design produces that assignment. The entries sum to 1 and are
in the same order as the columns of
[`obtain_permutation_matrix()`](https://declaredesign.org/r/randomizr/reference/obtain_permutation_matrix.md),
so the two can be used together.

## References

Andrews, G. E. (1976). *The Theory of Partitions*. Encyclopedia of
Mathematics and its Applications, Volume 2. Reading, MA: Addison-Wesley.

## See also

[`obtain_permutation_matrix()`](https://declaredesign.org/r/randomizr/reference/obtain_permutation_matrix.md),
[`obtain_num_permutations()`](https://declaredesign.org/r/randomizr/reference/obtain_num_permutations.md)

## Examples

``` r

# A design in which the possible assignments are *not* equally likely: with
# N = 5 and prob = 0.51, either 2 or 3 units are treated, and those two cases
# do not come up equally often.
declaration <- declare_ra(N = 5, prob_each = c(0.49, 0.51))

obtain_num_permutations(declaration)
#> [1] 20

perms <- obtain_permutation_matrix(declaration)
perm_probs <- obtain_permutation_probabilities(declaration)

# perms has one column per possible assignment and perm_probs has one entry
# per column, in the same order
dim(perms)
#> [1]  5 20
length(perm_probs)
#> [1] 20

# Each unit's probability of assignment to treatment, according to the
# declaration. Recovering these from perms is the check that the two objects
# line up.
true_probabilities <- declaration$probabilities_matrix[, 2]
true_probabilities
#> [1] 0.51 0.51 0.51 0.51 0.51

# The unweighted average across columns is WRONG here: it treats every
# assignment as equally likely, which this design does not.
rowMeans(perms)
#> [1] 0.5 0.5 0.5 0.5 0.5

# Weighting each column by how likely it is recovers the true probabilities.
# The matrix product does the weighted average: row i of perms times
# perm_probs sums unit i's treatment indicators weighted by column
# probability, which is exactly Pr(unit i treated).
perms %*% perm_probs
#>      [,1]
#> [1,] 0.51
#> [2,] 0.51
#> [3,] 0.51
#> [4,] 0.51
#> [5,] 0.51
```
