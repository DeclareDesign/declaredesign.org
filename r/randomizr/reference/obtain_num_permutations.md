# Obtain the Number of Possible Permutations from a Random Assignment Declaration

Obtain the Number of Possible Permutations from a Random Assignment
Declaration

## Usage

``` r
obtain_num_permutations(declaration)
```

## Arguments

- declaration:

  A random assignment or sampling declaration, created by
  [`declare_ra`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
  or
  [`declare_rs`](https://declaredesign.org/r/randomizr/reference/declare_rs.md).

## Value

a scalar

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
