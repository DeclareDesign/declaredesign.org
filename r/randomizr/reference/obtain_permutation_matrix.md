# Obtain Permutation Matrix from a Random Assignment Declaration

Enumerates the assignments a design could have produced, one column per
assignment. The matrix is the input to randomization inference, where a
test statistic is recomputed under each column to build the distribution
it would follow if the treatment had no effect. When a design admits
more assignments than `maximum_permutations`, a random sample of them is
returned instead, which approximates the same distribution.

## Usage

``` r
obtain_permutation_matrix(declaration, maximum_permutations = 10000)
```

## Arguments

- declaration:

  A random assignment declaration, created by
  [`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md).
  (required)

- maximum_permutations:

  If the number of possible random assignments exceeds
  `maximum_permutations`, `obtain_permutation_matrix` returns a random
  sample of `maximum_permutations` of them instead of enumerating all of
  them. Defaults to 10,000. (optional)

## Value

A matrix with one row per unit and one column per assignment, whose
entries are condition names. The columns are all of the assignments the
declared design could produce, or a random sample of
`maximum_permutations` of them if there are more than that. Column order
carries no meaning, but it is the order
[`obtain_permutation_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_permutation_probabilities.md)
returns its probabilities in.

## References

Andrews, G. E. (1976). *The Theory of Partitions*. Encyclopedia of
Mathematics and its Applications, Volume 2. Reading, MA: Addison-Wesley.

## See also

[`obtain_num_permutations()`](https://declaredesign.org/r/randomizr/reference/obtain_num_permutations.md),
[`obtain_permutation_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_permutation_probabilities.md),
[`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)

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
