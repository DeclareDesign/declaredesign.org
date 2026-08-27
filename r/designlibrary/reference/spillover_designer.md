# Create a design with spillovers

Builds a design with `N_groups` groups each containing `N_i_group`
individuals. Potential outcomes exhibit spillovers: if any individual in
a group receives treatment, the effect is spread equally among members
of the group.

## Usage

``` r
spillover_designer(
  N_groups = 80,
  N_i_group = 3,
  sd_i = 0.2,
  gamma = 2,
  args_to_fix = NULL
)
```

## Arguments

- N_groups:

  An integer. Number of groups.

- N_i_group:

  Number of units in each group. Can be scalar or vector of length
  `N_groups`.

- sd_i:

  A nonnegative number. Standard deviation of individual-level shock.

- gamma:

  A number. Parameter that controls whether spillovers within groups
  substitute or complement each other. See \`Details\`.

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A simple spillover design.

## Details

Parameter `gamma` controls interactions between spillover effects.For
`gamma`=1 for every \$1 given to a member of a group, each member
receives \$1\*`N_i_group` no matter how many others are already treated.
For `gamma`\>1 (\<1) for every \$1 given to a member of a group, each
member receives an amount that depends negatively (positively) on the
number already treated.

The default estimand is the average difference across subjects between
no one treated and only that subject treated.

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
# Generate a simple spillover design using default arguments:
spillover_design <- spillover_designer()
```
