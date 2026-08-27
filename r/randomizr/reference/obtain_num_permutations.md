# Obtain the Number of Possible Permutations from a Random Assignment Declaration

Counts the assignments a design could have produced. The count is the
size of the randomization distribution, so it says how much resolution a
randomization inference p-value can have: a design with 70 possible
assignments cannot produce a p-value below 1/70. Counting is exact and
cheap even when the number is far too large to enumerate, which is why
it is worth calling before
[`obtain_permutation_matrix()`](https://declaredesign.org/r/randomizr/reference/obtain_permutation_matrix.md).

## Usage

``` r
obtain_num_permutations(declaration)
```

## Arguments

- declaration:

  A random assignment or sampling declaration, created by
  [`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
  or
  [`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md).
  (required)

## Value

A single number: how many distinct assignments (or samples) the declared
design can produce. It can be far larger than any matrix you would want
to build, which is the point of counting first.

## See also

[`obtain_permutation_matrix()`](https://declaredesign.org/r/randomizr/reference/obtain_permutation_matrix.md),
[`obtain_permutation_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_permutation_probabilities.md),
[`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)

## Examples

``` r

# Random assignment
## complete

declaration <- declare_ra(N = 4)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1] 4 6
obtain_num_permutations(declaration)
#> [1] 6

## blocked

blocks <- c("A", "A", "B", "B", "C", "C", "C")
declaration <- declare_ra(blocks = blocks)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1]  7 24
obtain_num_permutations(declaration)
#> [1] 24

## clustered

clusters <- c("A", "B", "A", "B", "C", "C", "C")
declaration <- declare_ra(clusters = clusters)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1] 7 6
obtain_num_permutations(declaration)
#> [1] 6

## large

declaration <- declare_ra(20)
choose(20, 10)
#> [1] 184756
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1]    20 10000

# Random sampling
## complete

declaration <- declare_rs(N = 4)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1] 4 6
obtain_num_permutations(declaration)
#> [1] 6

## stratified

strata <- c("A", "A", "B", "B", "C", "C", "C")
declaration <- declare_rs(strata = strata)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1]  7 24
obtain_num_permutations(declaration)
#> [1] 24

## clustered

clusters <- c("A", "B", "A", "B", "C", "C", "C")
declaration <- declare_rs(clusters = clusters)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1] 7 6
obtain_num_permutations(declaration)
#> [1] 6

## large

declaration <- declare_rs(N = 20)
perms <- obtain_permutation_matrix(declaration)
dim(perms)
#> [1]    20 10000

```
