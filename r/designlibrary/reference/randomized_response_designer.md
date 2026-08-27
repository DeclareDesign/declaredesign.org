# Create a randomized response design

Produces a (forced) randomized response design that measures the share
of individuals with a given trait `prevalence_trait` in a population of
size `N`. Probability of forced response ("Yes") is given by
`prob_forced_yes`, and rate at which individuals with trait lie is given
by `withholding_rate`.

## Usage

``` r
randomized_response_designer(
  N = 1000,
  prob_forced_yes = 0.6,
  prevalence_rate = 0.1,
  withholding_rate = 0.5,
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Size of sample.

- prob_forced_yes:

  A number in \[0,1\]. Probability of a forced yes.

- prevalence_rate:

  A number in \[0,1\]. Probability that individual has the sensitive
  trait.

- withholding_rate:

  A number in \[0,1\]. Probability that an individual with the sensitive
  trait hides it.

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A randomized response design.

## Details

`randomized_response_designer` employs a specific variation of
randomized response designs in which respondents are required to report
a args_to_fix answer to the sensitive question with a given probability
(see Blair, Imai, and Zhou (2015) for alternative applications and
estimation strategies).

See [vignette
online](https://declaredesign.org/r/designlibrary/articles/randomized_response.html).

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
# Generate a randomized response design using default arguments:
randomized_response_design <- randomized_response_designer()
```
