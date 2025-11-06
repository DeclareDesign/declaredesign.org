# Creates panel or cross-classified data

This function allows the user to create data structures that are paneled
or cross-classified: where one level of observation draws simultaneously
from two or many source levels. Common examples of panels include
country-year data which have country-level and year-level
characteristics.

## Usage

``` r
cross_levels(by = NULL, ...)

link_levels(N = NULL, by = NULL, ...)
```

## Arguments

- by:

  The result of a call to
  [`join_using()`](https://declaredesign.org/r/fabricatr/reference/join_using.md)
  which specifies how the cross-classified data will be created

- ...:

  A variable or series of variables to add to the resulting data frame
  after the cross-classified data is created.

- N:

  The number of observations in the resulting data frame. If `N` is NULL
  or not provided, the join_using will be an "outer product" – merging
  each row of each provided data frame with each other data frame to
  make a full panel.

## Value

data.frame

## Details

By specifying the appropriate arguments in
[`join_using()`](https://declaredesign.org/r/fabricatr/reference/join_using.md)
within the function call, it is possible to induce correlation in
cross-classified data.

## Examples

``` r
# Generate full panel data
panel <- fabricate(
 countries = add_level(N = 20, country_shock = runif(N, 1, 10)),
 years = add_level(N = 20, year_shock = runif(N, 1, 10), nest=FALSE),
 obs = cross_levels(by = join_using(countries, years), GDP_it = country_shock + year_shock)
)

# Include an "N" argument to allow for cross-classified
# data.
students <- fabricate(
 primary_school = add_level(N = 20, ps_quality = runif(N, 1, 10)),
 secondary_school = add_level(N = 15, ss_quality = runif(N, 1, 10), nest=FALSE),
 students = link_levels(N = 500, by = join_using(primary_school, secondary_school))
)
head(students)
#>   primary_school ps_quality secondary_school ss_quality students
#> 1             11   9.265041               01   8.978955      001
#> 2             09   6.924900               07   9.075740      002
#> 3             07   1.629510               12   5.598684      003
#> 4             07   1.629510               07   9.075740      004
#> 5             12   2.338393               11   2.161146      005
#> 6             18   8.230713               12   5.598684      006

# Induce a correlation structure in cross-classified data by providing
# rho.
students <- fabricate(
 primary_school = add_level(N = 20, ps_quality = runif(N, 1, 10)),
 secondary_school = add_level(N = 15, ss_quality = runif(N, 1, 10), nest=FALSE),
 students = link_levels(N = 500, by = join_using(ps_quality, ss_quality, rho = 0.5))
)
cor(students$ps_quality, students$ss_quality)
#> [1] 0.4854366
```
