# Obtain Permutation Matrix from a Random Assignment Declaration

Obtain Permutation Matrix from a Random Assignment Declaration

## Usage

``` r
obtain_permutation_matrix(declaration, maximum_permutations = 10000)
```

## Arguments

- declaration:

  A random assignment declaration, created by
  [`declare_ra`](https://declaredesign.org/r/randomizr/reference/declare_ra.md).

- maximum_permutations:

  If the number of possible random assignments exceeds
  maximum_permutations, obtain_permutation_matrix will return a random
  sample of maximum_permutations permutations. Defaults to 10,000.

## Value

a matrix of all possible (or a random sample of all possible) random
assignments consistent with a declaration.

## Examples

``` r
# complete

declaration <- declare_ra(N = 4)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1] 4 6
obtain_num_permutations(declaration)
#> [1] 6

# blocked

blocks <- c("A", "A", "B", "B", "C", "C", "C")
declaration <- declare_ra(blocks = blocks)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1]  7 24
obtain_num_permutations(declaration)
#> [1] 24

# clustered

clusters <- c("A", "B", "A", "B", "C", "C", "C")
declaration <- declare_ra(clusters = clusters)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1] 7 6
obtain_num_permutations(declaration)
#> [1] 6

# large

declaration <- declare_ra(20)
choose(20, 10)
#> [1] 184756
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1]    20 10000

```
