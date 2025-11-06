# Extract model data for texreg package

Prepares a `"lm_robust"` or `"iv_robust"` object for the texreg package.
This is largely a clone of the `extract.lm` method.

## Usage

``` r
extract.robust_default(
  model,
  include.ci = TRUE,
  include.rsquared = TRUE,
  include.adjrs = TRUE,
  include.nobs = TRUE,
  include.fstatistic = FALSE,
  include.rmse = TRUE,
  include.nclusts = TRUE,
  ...
)

extract.lm_robust(
  model,
  include.ci = TRUE,
  include.rsquared = TRUE,
  include.adjrs = TRUE,
  include.nobs = TRUE,
  include.fstatistic = FALSE,
  include.rmse = TRUE,
  include.nclusts = TRUE,
  ...
)

extract.iv_robust(
  model,
  include.ci = TRUE,
  include.rsquared = TRUE,
  include.adjrs = TRUE,
  include.nobs = TRUE,
  include.fstatistic = FALSE,
  include.rmse = TRUE,
  include.nclusts = TRUE,
  ...
)
```

## Arguments

- model:

  an object of class
  [`lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
  or `"iv_robust"`

- include.ci:

  logical. Defaults to TRUE

- include.rsquared:

  logical. Defaults to TRUE

- include.adjrs:

  logical. Defaults to TRUE

- include.nobs:

  logical. Defaults to TRUE

- include.fstatistic:

  logical. Defaults to TRUE

- include.rmse:

  logical. Defaults to TRUE

- include.nclusts:

  logical. Defaults to TRUE if clusters in `model`

- ...:

  unused
