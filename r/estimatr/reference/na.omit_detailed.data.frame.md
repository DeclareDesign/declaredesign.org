# Extra logging on na.omit handler

Extra logging on na.omit handler

## Usage

``` r
na.omit_detailed.data.frame(object)
```

## Arguments

- object:

  a data.frame

## Value

a normal `omit` object, with the extra attribute `why_omit`, which
contains the leftmost column containing an NA for each row that was
dropped, by column name, if any were dropped.

## See also

[`na.omit`](https://rdrr.io/r/stats/na.fail.html)
