# Tidy an estimatr object

Tidy an estimatr object

## Usage

``` r
# S3 method for class 'lm_robust'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)

# S3 method for class 'iv_robust'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)

# S3 method for class 'difference_in_means'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)

# S3 method for class 'horvitz_thompson'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)

# S3 method for class 'lh_robust'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)

# S3 method for class 'lh'
tidy(x, conf.int = TRUE, conf.level = NULL, ...)
```

## Arguments

- x:

  An object returned by one of the estimators

- conf.int:

  Logical indicating whether or not to include a confidence interval in
  the tidied output. Defaults to ‘TRUE’.

- conf.level:

  The confidence level to use for the confidence interval if ‘conf.int =
  TRUE’. Must be strictly greater than 0 and less than 1. Defaults to
  0.95, which corresponds to a 95 percent confidence interval.

- ...:

  extra arguments (not used)

## Value

A data.frame with columns for coefficient names, estimates, standard
errors, confidence intervals, p-values, degrees of freedom, and the name
of the outcome variable

## See also

[`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md),
[`iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.md),
[`difference_in_means()`](https://declaredesign.org/r/estimatr/reference/difference_in_means.md),
[`horvitz_thompson()`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md)
