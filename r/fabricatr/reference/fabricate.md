# Fabricate a data frame

The main entry point for fabricatr. Builds a data frame column by
column, supporting hierarchical levels, multi-column outputs, and
starting from existing data.

## Usage

``` r
fabricate(..., N = NULL, ID_label = "ID", data = NULL)
```

## Arguments

- ...:

  Column definitions or level calls (`add_level`, `nest_level`,
  `declare_level`, `import_level`, `cross_levels`, `link_levels`,
  `modify_level`, `potential_outcomes`, `draw_multivariate`). `N` must
  be supplied by name.

- N:

  Number of rows. Made available as a scalar integer `N` inside all
  column expressions; it does not appear as a column in the output
  unless you write it explicitly (e.g. `n_obs = N`).

- ID_label:

  Name of the unit ID column created alongside `N`. Defaults to `"ID"`;
  set to `NA` to suppress it. Ignored when `data` is supplied or when
  the data frame is built from levels (each level's ID column is named
  after the level).

- data:

  Optional existing data frame to start from.

## Value

A tibble with one row per unit of the lowest level and columns in the
order they were declared. Each level contributes an ID column named
after the argument it was assigned to, placed before that level's own
columns; a flat call contributes one named by `ID_label`, and
`ID_label = NA` suppresses it. A call that starts from `data` adds no ID
column at all. A nested level's IDs are unique across the whole frame
rather than restarting inside each parent, so nesting 2 citizens in each
of 3 villages numbers the citizens `"1"` through `"6"`.

## ID column type

ID columns are zero-padded character strings, as in fabricatr 1.x, so
that an ID is a label rather than a quantity. The padding keeps
character sort order matching numeric order, and the character type
keeps a cluster ID from being read as a linear term by a model formula.
The padding width is set by the number of units at that level, so a
frame can hold `clusters` running `"1"` to `"4"` alongside `units`
running `"01"` to `"12"`.

## Counting the rows a level is building

`N` names the number of rows the current level is building, so
`rnorm(N)` draws one value per row. `n()` answers the same question as a
function call and is available in every declaration. Two things make it
worth reaching for. It cannot be confused with a parameter of the
design: `N` is a name, so a design that also has an `N` in the workspace
can leave a reader unsure which one an expression reads, and
`redesign()` rebinds names, where `n()` always means the rows in hand.
And a data set carrying a column called `n` keeps it: `sum(n)` reads the
column and `n()` reads the count.

## Examples

``` r
# Flat fabrication
fabricate(N = 100, Y = rnorm(N), X = rbinom(N, 1, 0.5))
#> # A tibble: 100 × 3
#>    ID          Y     X
#>    <chr>   <dbl> <int>
#>  1 001    0.290      1
#>  2 002    1.79       0
#>  3 003    0.341      1
#>  4 004   -0.242      1
#>  5 005    0.613      1
#>  6 006    0.0774     1
#>  7 007    1.78       0
#>  8 008    1.15       0
#>  9 009    0.732      1
#> 10 010   -0.642      1
#> # ℹ 90 more rows

# n() is the number of rows the level is building
fabricate(N = 10, Y = rnorm(n()))
#> # A tibble: 10 × 2
#>    ID          Y
#>    <chr>   <dbl>
#>  1 01    -0.941 
#>  2 02    -0.768 
#>  3 03    -0.0274
#>  4 04     0.103 
#>  5 05    -0.0119
#>  6 06     0.0767
#>  7 07     0.369 
#>  8 08    -0.205 
#>  9 09    -0.479 
#> 10 10     1.28  

# Hierarchical: 20 villages, 5 citizens each
fabricate(
  villages = add_level(N = 20, v_income = rnorm(N)),
  citizens = nest_level(N = 5, income = v_income + rnorm(N))
)
#> # A tibble: 100 × 4
#>    villages v_income citizens  income
#>    <chr>       <dbl> <chr>      <dbl>
#>  1 01          1.07  001       2.37  
#>  2 01          1.07  002       1.45  
#>  3 01          1.07  003       2.01  
#>  4 01          1.07  004       0.0224
#>  5 01          1.07  005       1.79  
#>  6 02          0.356 006      -2.25  
#>  7 02          0.356 007      -0.0328
#>  8 02          0.356 008      -0.276 
#>  9 02          0.356 009      -0.0298
#> 10 02          0.356 010       1.08  
#> # ℹ 90 more rows

# Starting from existing data
df <- data.frame(x = 1:10)
fabricate(data = df, y = x ^ 2)
#> # A tibble: 10 × 2
#>        x     y
#>    <int> <dbl>
#>  1     1     1
#>  2     2     4
#>  3     3     9
#>  4     4    16
#>  5     5    25
#>  6     6    36
#>  7     7    49
#>  8     8    64
#>  9     9    81
#> 10    10   100
```
