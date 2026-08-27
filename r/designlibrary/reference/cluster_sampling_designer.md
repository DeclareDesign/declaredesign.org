# Create a design for cluster random sampling

Builds a cluster sampling design for an ordinal outcome variable for a
population with `N_blocks` strata, each with `N_clusters_in_block`
clusters, each of which contains `N_i_in_cluster` units. The sampling
strategy involves sampling `n_clusters_in_block` clusters in each
stratum, and then sampling `n_i_in_cluster` units in each cluster.
Outcomes within clusters have intra-cluster correlation approximately
equal to `ICC`.

## Usage

``` r
cluster_sampling_designer(
  N_blocks = 1,
  N_clusters_in_block = 1000,
  N_i_in_cluster = 50,
  n_clusters_in_block = 100,
  n_i_in_cluster = 10,
  icc = 0.2,
  args_to_fix = NULL
)
```

## Arguments

- N_blocks:

  An integer. Number of blocks (strata). Defaults to 1 for no blocks.

- N_clusters_in_block:

  An integer or vector of integers of length `N_blocks`. Number of
  clusters in each block in the population.

- N_i_in_cluster:

  An integer or vector of integers of length `sum(N_clusters_in_block)`.
  Number of units per cluster sampled.

- n_clusters_in_block:

  An integer. Number of clusters to sample in each block (stratum).

- n_i_in_cluster:

  An integer. Number of units to sample in each cluster.

- icc:

  A number in \[0,1\]. Intra-cluster Correlation Coefficient (ICC).

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A stratified cluster sampling design.

## Details

Key limitations: The design assumes a args_to_fix number of clusters
drawn in each stratum and a args_to_fix number of individuals drawn from
each cluster.

See [vignette
online](https://declaredesign.org/r/designlibrary/articles/cluster_sampling.html).

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
# To make a design using default arguments:
cluster_sampling_design <- cluster_sampling_designer()
# A design with varying block size and varying cluster size
cluster_sampling_design <- cluster_sampling_designer(
  N_blocks = 2, N_clusters_in_block = 6:7, N_i_in_cluster = 3:15, 
  n_clusters_in_block = 5,  n_i_in_cluster = 2)
```
