# Internal method that creates linear fits

Internal method that creates linear fits

## Usage

``` r
lm_robust_fit(
  y,
  X,
  yoriginal = NULL,
  Xoriginal = NULL,
  weights,
  cluster,
  fixed_effects = NULL,
  ci = TRUE,
  se_type,
  has_int,
  alpha = 0.05,
  return_vcov = TRUE,
  return_fit = TRUE,
  try_cholesky = FALSE,
  iv_stage = list(0)
)
```

## Arguments

- y:

  numeric outcome vector

- X:

  numeric design matrix

- yoriginal:

  numeric outcome vector, unprojected if there are fixed effects

- Xoriginal:

  numeric design matrix, unprojected if there are fixed effects. Any
  column named `"(Intercept)" will be dropped`

- weights:

  numeric weights vector

- cluster:

  numeric cluster vector

- fixed_effects:

  character matrix of fixed effect groups

- ci:

  boolean that when T returns confidence intervals and p-values

- se_type:

  character denoting which kind of SEs to return

- has_int:

  logical, whether the model has an intercept, used for \\R^2\\

- alpha:

  numeric denoting the test size for confidence intervals

- return_vcov:

  logical, whether to return the vcov matrix for later usage

- return_fit:

  logical, whether to return fitted values

- try_cholesky:

  logical, whether to try using a cholesky decomposition to solve LS
  instead of a QR decomposition

- iv_stage:

  list of length two, the first element denotes the stage of 2SLS IV
  estimation, where 0 is used for OLS. The second element is only used
  for the second stage of 2SLS and has the first stage design matrix.
  For OLS, the default, `list(0)`, for the first stage of 2SLS
  `list(1)`, for second stage of 2SLS `list(2, first_stage_design_mat)`.
