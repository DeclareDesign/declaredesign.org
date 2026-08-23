# Draw normal data with fixed intra-cluster correlation.

Data is generated to ensure inter-cluster correlation 0, intra-cluster
correlation in expectation ICC. The data generating process used in this
function is specified at the following URL:
<https://stats.stackexchange.com/questions/263451/create-synthetic-data-with-a-given-intraclass-correlation-coefficient-icc>

## Usage

``` r
draw_normal_icc(
  mean = 0,
  N = NULL,
  clusters,
  sd = NULL,
  sd_between = NULL,
  total_sd = NULL,
  ICC = NULL
)
```

## Arguments

- mean:

  A number or vector of numbers, one mean per cluster. If none is
  provided, will default to 0.

- N:

  (Optional) A number indicating the number of observations to be
  generated. Must be equal to length(clusters) if provided.

- clusters:

  A vector of factors or items that can be coerced to clusters; the
  length will determine the length of the generated data.

- sd:

  A number or vector of numbers, indicating the standard deviation of
  each cluster's error terms – standard deviation within a cluster
  (default 1)

- sd_between:

  A number or vector of numbers, indicating the standard deviation
  between clusters.

- total_sd:

  A number indicating the total sd of the resulting variable. May only
  be specified if ICC is specified and `sd` and `sd_between` are not.

- ICC:

  A number indicating the desired ICC.

## Value

A vector of numbers corresponding to the observations from the supplied
cluster IDs.

## Details

The typical use for this function is for a user to provide an `ICC` and,
optionally, a set of within-cluster standard deviations, `sd`. If the
user does not provide `sd`, the default value is 1. These arguments
imply a fixed between-cluster standard deviation.

An alternate mode for the function is to provide between-cluster
standard deviations, `sd_between`, and an `ICC`. These arguments imply a
fixed within-cluster standard deviation.

If users provide all three of `ICC`, `sd_between`, and `sd`, the
function will warn the user and use the provided standard deviations for
generating the data.

## Examples

``` r

# Divide observations into clusters
clusters = rep(1:5, 10)

# Default: unit variance within each cluster
draw_normal_icc(clusters = clusters, ICC = 0.5)
#>  [1]  0.77929066  0.05583622  1.04689839  3.36319999 -2.59597153  1.04561825
#>  [7]  0.82163758  1.24334265  3.04262249 -2.74707159  1.57442449  0.67303177
#> [13]  1.57804119  3.84885169 -2.75790282  0.52852726  1.84551872  1.03838235
#> [19]  3.66877805 -1.41398361  1.46490210  0.11321516  0.09879596  1.66089943
#> [25] -1.00929482  2.19397503  1.54224420  0.84164257  1.85520460 -1.90019576
#> [31] -0.41237712 -0.67659904  0.46734573  1.86419632 -1.88546682  0.88179600
#> [37]  1.69593274  1.04824219  0.78111337 -1.77618796  1.01926615 -0.38585976
#> [43]  1.15376594  2.56632738 -0.36995741 -0.28874719  2.62144987  0.37521052
#> [49]  2.94565335  0.28079408

# Alternatively, you can specify characteristics:
draw_normal_icc(mean = 10, clusters = clusters, sd = 3, ICC = 0.3)
#>  [1] 12.536573 12.512369  6.264494 12.579453  8.875538 17.724870 11.870808
#>  [8]  6.034366  8.401600  5.981980 12.964939  1.954819  8.040242  8.336206
#> [15] 11.692609 12.643058  4.998140  9.493022  5.936959  5.519373 10.950054
#> [22]  5.879818  7.170970 11.641103 13.082030 16.295629  4.979014  9.903956
#> [29] 10.015819 10.082598 12.990313 10.520855 11.916609  9.700550 10.290251
#> [36] 10.913594  7.057740  9.804318 14.052711  9.717378 16.768930  9.729655
#> [43]  7.145765  9.895259 17.023143 17.341153  8.050773  9.739234  9.172570
#> [50]  9.063279

# Can specify between-cluster standard deviation instead:
draw_normal_icc(clusters = clusters, sd_between = 4, ICC = 0.2)
#>  [1]   2.66821201  -4.85442351   5.30094093  -2.73595470 -14.79487760
#>  [6]  -1.03404195   1.25861114   5.34240269   4.39590760  -3.44202658
#> [11]  -1.11618590  14.40295961   0.01523843  17.14638411   1.62678724
#> [16]   0.66215800  13.11537623  -0.67788429   7.11582478 -13.73691444
#> [21]  -8.30442396  15.28768008  16.71199017  -0.40354873   5.49352915
#> [26]  10.88601210  -4.99637516  -7.29237048   2.57863333   4.98388182
#> [31]   9.95105479   8.83458645  22.92377262  -2.83865284  -0.03082492
#> [36]  11.00291619 -12.01902386   4.35340453   5.83343187   9.50098190
#> [41]   3.65873337  -0.11767330  11.12115921   4.69453929   8.81203728
#> [46]   8.58032347   2.64183594  21.50894009   1.95811056 -15.45208367

# Can specify total SD instead:
total_sd_draw = draw_normal_icc(clusters = clusters, ICC = 0.5, total_sd = 3)
sd(total_sd_draw)
#> [1] 3

# Verify that ICC generated is accurate
corr_draw = draw_normal_icc(clusters = clusters, ICC = 0.4)
summary(lm(corr_draw ~ as.factor(clusters)))$r.squared
#> [1] 0.4785981
```
