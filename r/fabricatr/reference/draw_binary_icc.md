# Draw binary data with a target intra-cluster correlation

Uses the mixture-of-Bernoullis model: with probability \\\sqrt{ICC}\\
each unit takes the cluster-level draw; otherwise it takes an
independent draw. This produces an expected ICC of the specified value.

## Usage

``` r
draw_binary_icc(clusters, prob = 0.5, ICC = 0, N = NULL)
```

## Arguments

- clusters:

  Vector of cluster IDs.

- prob:

  Cluster success probability. Scalar or per-cluster vector. Default
  0.5.

- ICC:

  Target ICC. Default 0 (independent draws).

- N:

  Optional; must equal `length(clusters)`.

## Value

An integer vector of 0s and 1s of the same length as `clusters`, in the
order the clusters were given.

## Examples

``` r
clusters <- rep(1:10, each = 20)
y <- draw_binary_icc(clusters = clusters, prob = 0.5, ICC = 0.3)
summary(lm(y ~ factor(clusters)))$r.squared  # approx 0.3
#> [1] 0.312
```
