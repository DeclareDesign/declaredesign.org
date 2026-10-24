# Import an existing data frame as a level

Brings a data frame into a `fabricate` call as a level of its own, so
that real data can be crossed or linked with other real data.
`fabricate(data = )` starts from one frame and builds down from it;
`import_level` makes each frame a named level, which is the thing
`cross_levels` and `link_levels` work on.

## Usage

``` r
import_level(data, ..., .id = NULL)
```

## Arguments

- data:

  A data frame. Its rows are the level's units.

- ...:

  Column expressions evaluated against the imported columns, as in any
  other level. `N` is the number of rows imported.

- .id:

  Optional string naming the column of `data` that already identifies
  its rows. Left off, a column named after the level is used when the
  data has one, the way a join matches on a shared name, and otherwise
  fabricatr numbers the rows itself.

## Value

A `fabricatr_level` object, meaningful only as a named argument to
`fabricate`. It contributes the imported rows, an ID column named after
the argument it is assigned to, and one column per expression in `...`.
Like `declare_level`, it stands alone rather than nesting into the frame
in hand, so it can be named in a later `cross_levels` or `link_levels`.

## Which column is the ID

A level's ID column is named after the level, and `import_level` only
decides what goes in it. Three cases, in order:

- `.id = "pid"` uses that column of the imported data, renamed to the
  level's name. It has to exist, and its values have to be distinct and
  non-missing, because a level's rows are its units.

- Left off, a column already named after the level is used as it stands:
  `individuals = import_level(df)` where `df` has an `individuals`
  column keeps that column rather than putting a second ID beside it.

- Otherwise the rows are numbered, as `add_level` numbers them.

A column called `N` is refused. `N` is the number of rows the level is
building in every expression it evaluates, so the two cannot both be in
view; rename the column before importing it, or use
`fabricate(data = )`, which evaluates nothing at a level and keeps it.

An imported ID keeps its own type and values, where a fabricated one is
a zero-padded character string. That is the point of importing it: the
column still matches the same key in the data it came from. It also
means an integer key stays an integer, so a model formula reading it
will treat it as a quantity unless you make it a factor.

## Examples

``` r
individuals <- data.frame(individuals = c("ann", "bob", "cyd"),
                          ind_shock = c(-0.4, 0.1, 0.8))
periods <- data.frame(year = 2020:2022, period_shock = c(0.2, -0.1, 0.3))

# Each frame keeps its own key: `individuals` by name, `year` by `.id`
fabricate(
  individuals = import_level(individuals),
  period      = import_level(periods, .id = "year"),
  obs         = cross_levels(
    .by = c("individuals", "period"),
    Y = ind_shock + period_shock + rnorm(N)
  )
)
#> # A tibble: 9 × 6
#>   individuals ind_shock period period_shock obs         Y
#>   <chr>           <dbl>  <int>        <dbl> <chr>   <dbl>
#> 1 ann              -0.4   2020          0.2 1     -0.249 
#> 2 bob               0.1   2020          0.2 2     -0.926 
#> 3 cyd               0.8   2020          0.2 3      0.0342
#> 4 ann              -0.4   2021         -0.1 4     -0.829 
#> 5 bob               0.1   2021         -0.1 5      0.170 
#> 6 cyd               0.8   2021         -0.1 6      2.20  
#> 7 ann              -0.4   2022          0.3 7     -0.648 
#> 8 bob               0.1   2022          0.3 8     -0.582 
#> 9 cyd               0.8   2022          0.3 9      0.0863

# Imported units with fabricated units nested inside them
fabricate(
  villages = import_level(data.frame(villages = c("v1", "v2"),
                                     v_income = c(10, 12))),
  citizens = nest_level(N = 3, income = v_income + rnorm(N))
)
#> # A tibble: 6 × 4
#>   villages v_income citizens income
#>   <chr>       <dbl> <chr>     <dbl>
#> 1 v1             10 1          9.53
#> 2 v1             10 2          9.09
#> 3 v1             10 3         10.4 
#> 4 v2             12 4         12.6 
#> 5 v2             12 5         11.4 
#> 6 v2             12 6         12.5 
```
