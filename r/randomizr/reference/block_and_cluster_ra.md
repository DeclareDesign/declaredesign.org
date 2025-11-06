# Blocked and Clustered Random Assignment

A random assignment procedure in which units are assigned as clusters
and clusters are nested within blocks.

## Usage

``` r
block_and_cluster_ra(
  blocks = NULL,
  clusters = NULL,
  prob = NULL,
  prob_unit = NULL,
  prob_each = NULL,
  m = NULL,
  m_unit = NULL,
  block_m = NULL,
  block_m_each = NULL,
  block_prob = NULL,
  block_prob_each = NULL,
  num_arms = NULL,
  conditions = NULL,
  check_inputs = TRUE
)
```

## Arguments

- blocks:

  A vector of length N that indicates which block each unit belongs to.

- clusters:

  A vector of length N that indicates which cluster each unit belongs
  to.

- prob:

  Use for a two-arm design in which either floor(N_clusters_block\*prob)
  or ceiling(N_clusters_block\*prob) clusters are assigned to treatment
  within each block. The probability of assignment to treatment is
  exactly prob because with probability 1-prob,
  floor(N_clusters_block\*prob) clusters will be assigned to treatment
  and with probability prob, ceiling(N_clusters_block\*prob) clusters
  will be assigned to treatment. prob must be a real number between 0
  and 1 inclusive. (optional)

- prob_unit:

  Use for a two arm design. Must of be of length N. tapply(prob_unit,
  blocks, unique) will be passed to `block_prob`.

- prob_each:

  Use for a multi-arm design in which the values of prob_each determine
  the probabilities of assignment to each treatment condition. prob_each
  must be a numeric vector giving the probability of assignment to each
  condition. All entries must be nonnegative real numbers between 0 and
  1 inclusive and the total must sum to 1. Because of integer issues,
  the exact number of clusters assigned to each condition may differ
  (slightly) from assignment to assignment, but the overall probability
  of assignment is exactly prob_each. (optional)

- m:

  Use for a two-arm design in which the scalar m describes the fixed
  number of clusters assigned in each block. This number does not vary
  across blocks.

- m_unit:

  Use for a two-arm design. Must be of length N. tapply(m_unit, blocks,
  unique) will be passed to `block_m`.

- block_m:

  Use for a two-arm design in which block_m describes the number of
  clusters to assign to treatment within each block. block_m must be a
  numeric vector that is as long as the number of blocks and is in the
  same order as sort(unique(blocks)).

- block_m_each:

  Use for a multi-arm design in which the values of block_m_each
  determine the number of clusters assigned to each condition.
  block_m_each must be a matrix with the same number of rows as blocks
  and the same number of columns as treatment arms. Cell entries are the
  number of clusters to be assigned to each treatment arm within each
  block. The rows should respect the ordering of the blocks as
  determined by sort(unique(blocks)). The columns should be in the order
  of conditions, if specified.

- block_prob:

  Use for a two-arm design in which block_prob describes the probability
  of assignment to treatment within each block. Must be in the same
  order as sort(unique(blocks)). Differs from prob in that the
  probability of assignment can vary across blocks.

- block_prob_each:

  Use for a multi-arm design in which the values of block_prob_each
  determine the probabilities of assignment to each treatment condition.
  block_prob_each must be a matrix with the same number of rows as
  blocks and the same number of columns as treatment arms. Cell entries
  are the probabilities of assignment to treatment within each block.
  The rows should respect the ordering of the blocks as determined by
  sort(unique(blocks)). Use only if the probabilities of assignment
  should vary by block, otherwise use prob_each. Each row of
  block_prob_each must sum to 1.

- num_arms:

  The number of treatment arms. If unspecified, num_arms will be
  determined from the other arguments. (optional)

- conditions:

  A character vector giving the names of the treatment groups. If
  unspecified, the treatment groups will be named 0 (for control) and 1
  (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm
  trial. An exception is a two-group design in which num_arms is set to
  2, in which case the condition names are T1 and T2, as in a multi-arm
  trial with two arms. (optional)

- check_inputs:

  logical. Defaults to TRUE.

## Value

A vector of length N that indicates the treatment condition of each
unit.

## Examples

``` r
clusters <- rep(letters, times=1:26)

blocks <- rep(NA, length(clusters))
blocks[clusters %in% letters[1:5]] <- "block_1"
blocks[clusters %in% letters[6:10]] <- "block_2"
blocks[clusters %in% letters[11:15]] <- "block_3"
blocks[clusters %in% letters[16:20]] <- "block_4"
blocks[clusters %in% letters[21:26]] <- "block_5"


table(blocks, clusters)
#>          clusters
#> blocks     a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w
#>   block_1  1  2  3  4  5  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   block_2  0  0  0  0  0  6  7  8  9 10  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   block_3  0  0  0  0  0  0  0  0  0  0 11 12 13 14 15  0  0  0  0  0  0  0  0
#>   block_4  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 16 17 18 19 20  0  0  0
#>   block_5  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 21 22 23
#>          clusters
#> blocks     x  y  z
#>   block_1  0  0  0
#>   block_2  0  0  0
#>   block_3  0  0  0
#>   block_4  0  0  0
#>   block_5 24 25 26

Z <- block_and_cluster_ra(blocks = blocks,
                          clusters = clusters)

table(Z, blocks)
#>    blocks
#> Z   block_1 block_2 block_3 block_4 block_5
#>   0       7      23      26      56      72
#>   1       8      17      39      34      69
table(Z, clusters)
#>    clusters
#> Z    a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   0  1  2  0  4  0  6  7  0  0 10  0 12  0 14  0  0 17  0 19 20 21  0  0  0 25
#>   1  0  0  3  0  5  0  0  8  9  0 11  0 13  0 15 16  0 18  0  0  0 22 23 24  0
#>    clusters
#> Z    z
#>   0 26
#>   1  0

Z <- block_and_cluster_ra(blocks = blocks,
                          clusters = clusters,
                          num_arms = 3)

table(Z, blocks)
#>     blocks
#> Z    block_1 block_2 block_3 block_4 block_5
#>   T1       7      17      11      35      51
#>   T2       3       7      26      35      43
#>   T3       5      16      28      20      47
table(Z, clusters)
#>     clusters
#> Z     a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   T1  0  0  3  4  0  0  0  8  9  0 11  0  0  0  0  0 17 18  0  0  0  0  0  0 25
#>   T2  1  2  0  0  0  0  7  0  0  0  0 12  0 14  0 16  0  0 19  0 21 22  0  0  0
#>   T3  0  0  0  0  5  6  0  0  0 10  0  0 13  0 15  0  0  0  0 20  0  0 23 24  0
#>     clusters
#> Z     z
#>   T1 26
#>   T2  0
#>   T3  0

Z <- block_and_cluster_ra(blocks = blocks,
                          clusters = clusters,
                          prob_each = c(.2, .5, .3))

block_m_each <- rbind(c(2, 3),
                      c(1, 4),
                      c(3, 2),
                      c(2, 3),
                      c(5, 1))

Z <- block_and_cluster_ra(blocks = blocks,
                          clusters = clusters,
                          block_m_each = block_m_each)

table(Z, blocks)
#>    blocks
#> Z   block_1 block_2 block_3 block_4 block_5
#>   0       6       6      38      34     116
#>   1       9      34      27      56      25
table(Z, clusters)
#>    clusters
#> Z    a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y
#>   0  0  2  0  4  0  6  0  0  0  0 11 12  0  0 15 16  0 18  0  0 21 22 23 24  0
#>   1  1  0  3  0  5  0  7  8  9 10  0  0 13 14  0  0 17  0 19 20  0  0  0  0 25
#>    clusters
#> Z    z
#>   0 26
#>   1  0
```
