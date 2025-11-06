# Expands data to a given length through recycling.

This function is a helper function designed call `rep_len` to expand the
length of a data vector, but which can dynamically retrieve N from the
surrounding level call for use in fabricatr.

## Usage

``` r
recycle(x, .N = NULL)
```

## Arguments

- x:

  Data to recycle into length `N`

- .N:

  the length to recycle the data to, typically provided implicitly by a
  or fabricate call wrapped around the function call.

## Value

A vector of data padded to length `N`

## Examples

``` r
fabricate(
  N = 15,
  month = recycle(month.abb)
)
#>    ID month
#> 1  01   Jan
#> 2  02   Feb
#> 3  03   Mar
#> 4  04   Apr
#> 5  05   May
#> 6  06   Jun
#> 7  07   Jul
#> 8  08   Aug
#> 9  09   Sep
#> 10 10   Oct
#> 11 11   Nov
#> 12 12   Dec
#> 13 13   Jan
#> 14 14   Feb
#> 15 15   Mar
```
