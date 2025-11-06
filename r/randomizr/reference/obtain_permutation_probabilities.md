# Obtain the probabilities of permutations

Obtain the probabilities of permutations

## Usage

``` r
obtain_permutation_probabilities(declaration)
```

## Arguments

- declaration:

  A random assignment declaration, created by
  [`declare_ra`](https://declaredesign.org/r/randomizr/reference/declare_ra.md).

## Value

a vector of probabilities

## Examples

``` r
declaration <- declare_ra(N = 5, prob_each = c(.49, .51))
obtain_num_permutations(declaration)
#> [1] 20
perm_probs <- obtain_permutation_probabilities(declaration)
perms <- obtain_permutation_matrix(declaration)

# probabilities of assignment from declaration *should* match the average over all permutations
true_probabilities <- declaration$probabilities_matrix[,2]
true_probabilities
#> [1] 0.51 0.51 0.51 0.51 0.51

# correctly WRONG because the perms have different probs!
rowMeans(perms)
#> [1] 0.5 0.5 0.5 0.5 0.5

# correctly correct!
perms %*% perm_probs
#>      [,1]
#> [1,] 0.51
#> [2,] 0.51
#> [3,] 0.51
#> [4,] 0.51
#> [5,] 0.51
```
