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
#>  [1]  0.10145656  0.95743798  1.65911680 -2.54998986  0.69372488  1.27394351
#>  [7]  0.41777915  1.47904316 -1.20607065  1.63009972 -0.45836005 -0.52180725
#> [13] -0.52883545 -0.80138186  2.35917265  0.97066899  0.22103936 -0.33453029
#> [19] -1.69228280 -0.24717950 -1.24817425 -0.15325748 -0.32553857 -1.67755386
#> [25]  1.04699362  1.12435753  0.42763898 -1.40862151 -1.56827500  1.18446377
#> [31] -0.95743497  0.53316273  0.37659250 -0.16204445 -0.12354957  2.04987466
#> [37] -0.24539268  0.75591847  0.48870703  2.14372084 -0.45885401 -0.01632266
#> [43]  0.57168507 -1.05547818  0.88136572  1.34473027 -0.71927392  1.19117878
#> [49] -1.78456192  2.61079802

# Alternatively, you can specify characteristics:
draw_normal_icc(mean = 10, clusters = clusters, sd = 3, ICC = 0.3)
#>  [1]  5.076988  6.509019  6.817821  8.366439 10.444273  8.120309  7.961799
#>  [8]  4.418574  2.193203  8.751269  9.001987  5.639747 10.122718  9.755860
#> [15] 14.096844  8.101183  8.372733  8.497434  6.756428 10.791528 13.643024
#> [22] 10.385386  8.182165  6.964080  8.714809 10.179909  8.273095 12.534325
#> [29]  6.391207 14.570145 12.851823  5.614542  8.376873 13.696973 15.142368
#> [36] 11.172942  8.208011  7.654184  5.737109 13.895559 14.859460 11.122936
#> [43]  8.927466  6.881068  9.507486  8.713022  8.164188  7.660578  1.652861
#> [50]  8.119141

# Can specify between-cluster standard deviation instead:
draw_normal_icc(clusters = clusters, sd_between = 4, ICC = 0.2)
#>  [1]   9.2642270  -3.9090275  19.0231307   0.3322693  -7.0808044   7.9766436
#>  [7]  -4.6021503   8.9925713 -15.0314324 -16.0473864  10.1489475  12.7877242
#> [13]   1.4731978   4.1990112   3.1430497 -10.1351078 -11.2166364   4.4553799
#> [19]   3.6893639   2.2080924   3.6958538  18.9995067  -0.9619063  -1.3253429
#> [25]   3.2599538 -17.1577565   0.4291386   7.7101784   8.2064640  -4.0842291
#> [31]  -5.2564059   7.1968932   6.5712858   7.5175193   0.8373610  -2.4968967
#> [37]  17.5846741   3.8348571 -16.7466016  -4.0949566  10.4060321   9.0699692
#> [43]   7.3313381  -2.5639174 -10.3908239  10.8390784  -0.1416314  -0.8674508
#> [49]   3.2710132   9.2608545

# Can specify total SD instead:
total_sd_draw = draw_normal_icc(clusters = clusters, ICC = 0.5, total_sd = 3)
sd(total_sd_draw)
#> [1] 3

# Verify that ICC generated is accurate
corr_draw = draw_normal_icc(clusters = clusters, ICC = 0.4)
summary(lm(corr_draw ~ as.factor(clusters)))$r.squared
#> [1] 0.7263293
```
