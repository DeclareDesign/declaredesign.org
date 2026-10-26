# Build a diagnosands step from a set of named diagnosands

Two ways to call this, told apart by whether the first argument is a
`design_step`.

## Usage

``` r
select_diagnosands(..., alpha = 0.05, subset = NULL, na.rm = FALSE)
```

## Arguments

- ...:

  (required) Diagnosand names to keep, as strings, optionally preceded
  by a diagnosands `design_step` to subset.

- alpha:

  (optional) Significance level used by `power`, `type_s_rate`,
  `exaggeration_ratio`, and `prop_pos_sig`. Defaults to `0.05`.

- subset:

  (optional) An expression evaluated on the simulations table; only the
  rows for which it is `TRUE` enter the diagnosands. `NULL` (the
  default) keeps every simulation.

- na.rm:

  (optional) Passed to the library diagnosands. Defaults to `FALSE`.

## Value

A diagnosands `design_step`, as
[`declare_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md)
returns, holding the named diagnosands in the order given.

## Details

Given names only, `select_diagnosands("bias", "power")` builds a fresh
diagnosands step out of the built-in library, which is what
DeclareDesign does. `alpha`, `subset`, and `na.rm` shape the expressions
it writes.

Given a diagnosands step first,
`select_diagnosands(my_diagnosands, "bias")` keeps those entries of that
step and nothing else. The expressions are yours and are copied
unchanged, so `alpha`, `subset`, and `na.rm` have nothing to act on and
are refused rather than ignored: set them on the
[`declare_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md)
call that made the set.

A consequence worth knowing: the same name can mean different things in
the two forms. `select_diagnosands("bias")` writes
`mean(estimate - estimand, na.rm = FALSE)`, following DeclareDesign's
default, while `select_diagnosands(default_diagnosands(), "bias")`
returns the `na.rm = TRUE` version
[`default_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md)
declares.

The library is a superset of
[`default_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md):
`mean_estimand`, `mean_estimate`, `bias`, `sd_estimate`, `rmse`,
`power`, `coverage`, `mean_se`, `type_s_rate`, `exaggeration_ratio`,
`var_estimate`, `mean_var_hat`, `prop_pos_sig`, and `mean_ci_length`.

## See also

Other diagnosands:
[`declare_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md),
[`default_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md),
[`format.diagnosis()`](https://declaredesign.org/r/declaredesign/reference/format.diagnosis.md),
[`set_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/set_diagnosands.md)

## Examples

``` r
select_diagnosands("sd_estimate", "mean_se")
#> <design_step: diagnosands [diagnosand]>

# subset a custom set instead of the library
select_diagnosands(default_diagnosands(), "bias", "rmse")
#> <design_step: diagnosands [diagnosand]>
```
