# Draw binary data with fixed intra-cluster correlation.

Data is generated to ensure inter-cluster correlation 0, intra-cluster
correlation in expectation ICC. Algorithm taken from Hossein, Akhtar.
"ICCbin: An R Package Facilitating Clustered Binary Data Generation, and
Estimation of Intracluster Correlation Coefficient (ICC) for Binary
Data".

## Usage

``` r
draw_binary_icc(prob = 0.5, N = NULL, clusters, ICC = 0)
```

## Arguments

- prob:

  A number or vector of numbers, one probability per cluster. If none is
  provided, will default to 0.5.

- N:

  (Optional) A number indicating the number of observations to be
  generated. Must be equal to length(clusters) if provided.

- clusters:

  A vector of factors or items that can be coerced to clusters; the
  length will determine the length of the generated data.

- ICC:

  A number indicating the desired `ICC`, if none is provided the default
  ICC will be 0.

## Value

A vector of binary numbers corresponding to the observations from the
supplied cluster IDs.

## Examples

``` r
# Divide units into clusters
clusters = rep(1:5, 10)

# Default probability 0.5, default ICC 0
draw_binary_icc(clusters = clusters)
#>  [1] 0 1 1 1 0 0 0 0 1 1 0 1 1 0 0 0 1 0 0 1 1 0 1 1 1 0 1 0 0 0 0 0 1 1 0 0 0 1
#> [39] 1 1 1 1 0 1 1 1 1 0 0 0

# Specify probability or ICC
corr_draw = draw_binary_icc(prob = 0.5, clusters = clusters, ICC = 0.5)

# Verify ICC of data.
summary(lm(corr_draw ~ as.factor(clusters)))$r.squared
#> [1] 0.6095238
```
