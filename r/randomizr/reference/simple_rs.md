# Simple Random Sampling

`simple_rs` draws a sample in which every unit is included or not
independently of the others, as a separate coin flip. Because the draws
are independent, the size of the realized sample varies from draw to
draw. For most applications in which the size of the sampling frame is
known in advance,
[`complete_rs()`](https://declaredesign.org/r/randomizr/reference/complete_rs.md)
is preferable because it fixes the number of units sampled.

## Usage

``` r
simple_rs(N, prob = NULL, prob_unit = NULL, check_inputs = TRUE, simple = TRUE)
```

## Arguments

- N:

  The number of units in the sampling frame. Must be a positive integer.
  (required)

- prob:

  The probability of being sampled; must be a real number between 0 and
  1 inclusive and of length 1. (optional)

- prob_unit:

  The probability of being sampled for each unit; must be a real number
  between 0 and 1 inclusive and of length N. Because units are drawn
  independently, this probability may differ from unit to unit.
  (optional)

- check_inputs:

  Logical. Whether to verify before sampling that the arguments are
  internally consistent: that probabilities lie between 0 and 1, that
  vectors are of length N, and that only one of `prob` and `prob_unit`
  is supplied. Defaults to `TRUE`. Set to `FALSE` to skip the checks
  when drawing many samples from arguments that have already been
  verified; declaring the design once with
  [`declare_rs()`](https://declaredesign.org/r/randomizr/reference/declare_rs.md)
  and drawing from it with
  [`draw_rs()`](https://declaredesign.org/r/randomizr/reference/draw_rs.md)
  does this for you. (optional)

- simple:

  Logical. Internal use only; leave at its default. `simple_rs` always
  draws units independently, and this argument exists so that the
  argument checker knows as much. (optional)

## Value

A numeric vector of length N indicating whether each unit is sampled (1)
or not (0).

## Details

If `prob` is not specified, each unit is sampled with probability 0.5.

## See also

[`complete_rs()`](https://declaredesign.org/r/randomizr/reference/complete_rs.md),
[`strata_rs()`](https://declaredesign.org/r/randomizr/reference/strata_rs.md),
[`simple_ra()`](https://declaredesign.org/r/randomizr/reference/simple_ra.md),
[`simple_rs_probabilities()`](https://declaredesign.org/r/randomizr/reference/simple_rs_probabilities.md)

## Examples

``` r

S <- simple_rs(N = 100)
table(S)
#> S
#>  0  1 
#> 49 51 

S <- simple_rs(N = 100, prob = 0.3)
table(S)
#> S
#>  0  1 
#> 66 34 

# A probability of inclusion that varies unit by unit
S <- simple_rs(N = 100, prob_unit = seq(0.1, 0.9, length.out = 100))
table(S)
#> S
#>  0  1 
#> 50 50 
```
