# Perform generation of a correlated random variable.

This function is EXPERIMENTAL, and we cannot guarantee its properties
for all data structures. Be sure to diagnose your design and assess the
distribution of your variables.

## Usage

``` r
correlate(draw_handler, ..., given, rho)
```

## Arguments

- draw_handler:

  The unquoted name of a function to generate data. Currently,
  `draw_binary`, `draw_binomial`, and `draw_count` are supported.

- ...:

  The arguments to draw_handler (e.g. `prob`, `mean`, etc.)

- given:

  A vector that can be ordered; the reference distribution X that Y will
  be correlated with.

- rho:

  A rank correlation coefficient between -1 and 1.

## Details

In order to generate a random variable of a specific distribution based
on another variable of any distribution and a correlation coefficient
`rho`, we map the first, known variable into the standard normal space
via affine transformation, generate the conditional distribution of the
resulting variable as a standard normal, and then map that standard
normal back to the target distribution. The result should ensure, in
expectation, a rank-order correlation of `rho`.

## Examples

``` r

# Generate a variable of interest
exam_score <- pmin(100, rnorm(n = 100, mean = 80, sd = 10))

# Generate a correlated variable using fabricatr variable generation
scholarship_offers <- correlate(given = exam_score, rho = 0.7,
                                draw_count, mean = 3)

# Generate a correlated variable using base R distributions
final_grade <- pmax(100, correlate(given = exam_score, rho = 0.7,
                                   rnorm, mean = 80, sd = 10))
```
