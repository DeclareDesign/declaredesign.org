# Fabricate from a list of quosures

The programmatic entry point, for a caller that has already captured the
expressions it wants fabricated. \[fabricate()\] captures its `...` with
`enquos()`, which such a caller cannot use: splicing captured quosures
back in with `!!!` renders them as formulas, and
[`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
then captures those, one level too deep. `fabricate_with_dots()` takes
the captured list as it stands and builds the frame \[fabricate()\]
would have built from the same expressions.

## Usage

``` r
fabricate_with_dots(data = NULL, dots, ID_label = "ID")
```

## Arguments

- data:

  Optional existing data frame to start from, as in \[fabricate()\].

- dots:

  A named list of quosures, such as \[rlang::quos()\] or
  \[rlang::enquos()\] returns. A quosure named `N` sets the number of
  rows and does not become a column, exactly as `fabricate(N = )` does.
  Every other element needs a name.

- ID_label:

  Name of the unit ID column, or `NA` to suppress it. See
  \[fabricate()\].

## Value

A tibble, the same one \[fabricate()\] returns for the same expressions.

## Details

DeclareDesign is the caller this exists for. Its `declare_model()`,
`declare_measurement()`, `declare_assignment()` and `declare_sampling()`
each capture their own arguments, so that a design carries its
expressions as a value rather than as a view onto the environment it was
written in, and then pass the captured list here.

## See also

\[fabricate()\], which is what to call when you hold the expressions
rather than their captures.

## Examples

``` r
dots <- rlang::quos(N = 10, Y = rnorm(N), X = Y > 0)
fabricate_with_dots(dots = dots)
#> # A tibble: 10 × 3
#>    ID         Y X    
#>    <chr>  <dbl> <lgl>
#>  1 01    -0.446 FALSE
#>  2 02     0.104 TRUE 
#>  3 03     1.41  TRUE 
#>  4 04     1.69  TRUE 
#>  5 05     2.29  TRUE 
#>  6 06     2.72  TRUE 
#>  7 07     2.17  TRUE 
#>  8 08     0.180 TRUE 
#>  9 09    -1.69  FALSE
#> 10 10    -0.789 FALSE

# The same frame without the row id, which is how a step that adds columns
# to data already in hand calls it.
fabricate_with_dots(dots = rlang::quos(Y2 = Y * 2),
                    data = fabricate_with_dots(dots = dots),
                    ID_label = NA)
#> # A tibble: 10 × 4
#>    ID         Y X         Y2
#>    <chr>  <dbl> <lgl>  <dbl>
#>  1 01    -0.870 FALSE -1.74 
#>  2 02    -1.25  FALSE -2.50 
#>  3 03    -0.279 FALSE -0.559
#>  4 04    -0.590 FALSE -1.18 
#>  5 05     1.18  TRUE   2.36 
#>  6 06    -0.917 FALSE -1.83 
#>  7 07     0.227 TRUE   0.453
#>  8 08    -0.435 FALSE -0.870
#>  9 09    -1.01  FALSE -2.02 
#> 10 10     0.828 TRUE   1.66 
```
