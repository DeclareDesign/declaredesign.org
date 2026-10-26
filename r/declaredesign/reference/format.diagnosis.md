# Format a diagnosis for display

Rounds the diagnosands, puts each bootstrap standard error in
parentheses on a second row beneath its estimate, and gives the columns
display names (`mean_estimand` becomes `Mean Estimand`). This is what
[`print.diagnosis()`](https://declaredesign.org/r/declaredesign/reference/print.diagnosis.md)
shows and what you want in front of
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html).

## Usage

``` r
# S3 method for class 'diagnosis'
format(x, digits = 2, ...)

reshape_diagnosis(diagnosis, digits = 2)
```

## Arguments

- x:

  (required) A `diagnosis` object.

- digits:

  (optional) Number of decimal places. Defaults to `2`.

- ...:

  (optional) Ignored.

- diagnosis:

  (required) A `diagnosis` object.

## Value

A `data.frame` of character columns: the grouping columns and one column
per diagnosand under its display name, with each bootstrap standard
error in parentheses on the row beneath its estimate.

## Details

The standard-error rows appear only when the diagnosis was bootstrapped.
At `bootstrap_sims = 0` there are no replicates, so the table is one row
per group and there are no parentheses.

It is a [`format()`](https://rdrr.io/r/base/format.html) method because
that is what it does. R's
[`format()`](https://rdrr.io/r/base/format.html) is the generic for "the
character representation this object prints as", and
[`print()`](https://rdrr.io/r/base/print.html) is documented as calling
it. The three views of a diagnosis are worth keeping straight:
[`get_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/get_diagnosands.md)
is wide and numeric with `se(bias)` as a sibling column,
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) is long and
numeric with the standard error and interval as fields of each row, and
this is wide and character with the standard error rendered beneath its
estimate. Only the first two can be computed on. This one is a rendering
of the first, a pure function of it and `digits`, and it carries
strictly less information than either: the interval is not in it and the
precision is gone.

`reshape_diagnosis()` is DeclareDesign's name for the same thing and
calls this method, so code written against DeclareDesign keeps working.

It returns a `data.frame` rather than a tibble because every column is a
formatted string, and a tibble would print a `<chr>` row under a display
table.

Redesign parameter columns keep their own names, since `prob_each` is
the argument the reader passed and not a phrase to be title-cased.

DeclareDesign's `select` and `exclude` arguments are deliberately
absent: the result is a data frame, so `select()` already chooses
columns from it, and an argument that duplicates a verb is the wart this
package exists to remove.

## See also

Other diagnosands:
[`declare_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md),
[`default_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md),
[`select_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/select_diagnosands.md),
[`set_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/set_diagnosands.md)

## Examples

``` r
design <- declare_model(N = 30, Y = rnorm(N), Z = rep(0:1, 15)) +
  declare_inquiry(ATE = 0) +
  declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE",
                    label = "ols")
d <- diagnose_design(design, sims = 5, bootstrap_sims = 0)
format(d)
#>   Design Inquiry Estimator Term N Sims Mean Estimand Mean Estimate  Bias
#> 1 design     ATE       ols    Z      5          0.00         -0.10 -0.10
#>   SD Estimate RMSE Power Coverage
#> 1        0.39 0.36  0.00     1.00

# DeclareDesign's name for the same table
reshape_diagnosis(d)
#>   Design Inquiry Estimator Term N Sims Mean Estimand Mean Estimate  Bias
#> 1 design     ATE       ols    Z      5          0.00         -0.10 -0.10
#>   SD Estimate RMSE Power Coverage
#> 1        0.39 0.36  0.00     1.00

# choose columns with select(), on the data frame it returns
format(d) |> dplyr::select(Term, Bias, Power)
#>   Term  Bias Power
#> 1    Z -0.10  0.00
```
