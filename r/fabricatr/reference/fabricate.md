# Fabricate data

`fabricate` helps you simulate a dataset before you collect it. You can
either start with your own data and add simulated variables to it (by
passing `data` to `fabricate()`) or start from scratch by defining `N`.
Create hierarchical data with multiple levels of data such as citizens
within cities within states using `add_level()` or modify existing
hierarchical data using `modify_level()`. You can use any R function to
create each variable. Use
[`cross_levels()`](https://declaredesign.org/r/fabricatr/reference/cross_levels.md)
and
[`link_levels()`](https://declaredesign.org/r/fabricatr/reference/cross_levels.md)
to make more complex designs such as panel or cross-classified data.

## Usage

``` r
fabricate(..., data = NULL, N = NULL, ID_label = NULL)

add_level(N = NULL, ..., nest = TRUE)

modify_level(..., by = NULL)

nest_level(N = NULL, ...)
```

## Arguments

- ...:

  Variable or level-generating arguments, such as `my_var = rnorm(N)`.
  For `fabricate`, you may also pass `add_level()` or `modify_level()`
  arguments, which define a level of a multi-level dataset. See
  examples.

- data:

  (optional) user-provided data that forms the basis of the fabrication,
  e.g. you can add variables to existing data. Provide either `N` or
  `data` (`N` is the number of rows of the data if `data` is provided).
  If `data` and `N` are not provided, fabricatr will try to interpret
  the first un-named argument as either `data` or `N` based on type.

- N:

  (optional) number of units to draw. If provided as `fabricate(N = 5)`,
  this determines the number of units in the single-level data. If
  provided in `add_level`, e.g. `fabricate(cities = add_level(N = 5))`,
  `N` determines the number of units in a specific level of a
  hierarchical dataset.

- ID_label:

  (optional) variable name for ID variable, e.g. citizen_ID. Set to NA
  to suppress the creation of an ID variable.

- nest:

  (Default TRUE) Boolean determining whether data in an `add_level()`
  call will be nested under the current working data frame or create a
  separate hierarchy of levels. See our vignette for cross-classified,
  non-nested data for details.

- by:

  (optional) quoted name of variable `modify_level` uses to
  split-modify-combine data by.

## Value

data.frame

## Details

We also provide several built-in options to easily create variables,
including
[`draw_binary`](https://declaredesign.org/r/fabricatr/reference/draw_discrete.md),
[`draw_count`](https://declaredesign.org/r/fabricatr/reference/draw_discrete.md),
[`draw_likert`](https://declaredesign.org/r/fabricatr/reference/draw_likert.md),
and intra-cluster correlated variables
[`draw_binary_icc`](https://declaredesign.org/r/fabricatr/reference/draw_binary_icc.md)
and
[`draw_normal_icc`](https://declaredesign.org/r/fabricatr/reference/draw_normal_icc.md)

## See also

[`link_levels`](https://declaredesign.org/r/fabricatr/reference/cross_levels.md)

## Examples

``` r


# Draw a single-level dataset with a covariate
building_df <- fabricate(
  N = 100,
  height_ft = runif(N, 300, 800)
)
head(building_df)
#>    ID height_ft
#> 1 001  640.3523
#> 2 002  776.1245
#> 3 003  392.9777
#> 4 004  440.5836
#> 5 005  550.4447
#> 6 006  318.7363

# Start with existing data instead
building_modified <- fabricate(
  data = building_df,
  rent = rnorm(N, mean = height_ft * 100, sd = height_ft * 30)
)

# Draw a two-level hierarchical dataset
# containing cities within regions
multi_level_df <- fabricate(
 regions = add_level(N = 5),
 cities = add_level(N = 2, pollution = rnorm(N, mean = 5)))
head(multi_level_df)
#>   regions cities pollution
#> 1       1     01  4.862222
#> 2       1     02  4.402063
#> 3       2     03  4.277927
#> 4       2     04  5.072762
#> 5       3     05  6.473363
#> 6       3     06  4.683794

# Start with existing data and add a nested level:
company_df <- fabricate(
 data = building_df,
 company_id = add_level(N=10, is_headquarters = sample(c(0, 1), N, replace=TRUE))
)

# Start with existing data and add variables to hierarchical data
# at levels which are already present in the existing data.
# Note: do not provide N when adding variables to an existing level
fabricate(
  data = multi_level_df,
  regions = modify_level(watershed = sample(c(0, 1), N, replace = TRUE)),
  cities = modify_level(runoff = rnorm(N))
)
#>    regions cities pollution watershed     runoff
#> 1        1     01  4.862222         1 -0.4299057
#> 2        1     02  4.402063         0 -0.6519163
#> 3        2     03  4.277927         1 -1.3227553
#> 4        2     04  5.072762         0 -0.6110024
#> 5        3     05  6.473363         1 -0.4334848
#> 6        3     06  4.683794         0  0.2211316
#> 7        4     07  3.585279         0  0.4146353
#> 8        4     08  3.013821         0  1.0035196
#> 9        5     09  5.705734         1 -2.6260422
#> 10       5     10  4.209572         0 -0.8735349

# fabricatr can add variables that are higher-level summaries of lower-level
# variables via a split-modify-combine logic and the \code{by} argument

multi_level_df <-
 fabricate(
   regions = add_level(N = 5, elevation = rnorm(N)),
   cities = add_level(N = 2, pollution = rnorm(N, mean = 5)),
   cities = modify_level(by = "regions", regional_pollution = mean(pollution))
 )

# fabricatr can also make panel or cross-classified data. For more
# information about syntax for this functionality please read our vignette
# or check documentation for \code{link_levels}:
cross_classified <- fabricate(
  primary_schools = add_level(N = 50, ps_quality = runif(N, 0, 10)),
  secondary_schools = add_level(N = 100, ss_quality = runif(N, 0, 10), nest=FALSE),
  students = link_levels(N = 2000,
                          by = join_using(ps_quality, ss_quality, rho = 0.5),
                          student_quality = ps_quality + 3*ss_quality + rnorm(N)))
```
