# Create a multi-arm design

Routes to
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)
with id `"multiarm_trial"`:
`make_design("multiarm_trial", N = N, m_arms = m_arms, ...)`. Defaults
for `outcome_means`, `outcome_sds`, and `conditions` are evaluated after
`m_arms`, so `multi_arm_designer(m_arms = 4)` expands those vectors
before calling
[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md).
`make_design("multiarm_trial", m_arms = 4)` does the same inside the
design: constant vectors are repeated and conditions `1..k` become
`1..m_arms`. Any other wrong-length vector errors. Inquiries are the
differences in arm means for every pair of arms, `ate_Y_j_k`, and each
is estimated by a difference in means between its two arms (estimator
`"DIM (Z_j - Z_k)"`). The contrasts between two treatment arms are
usually the least powered. Library-file knobs match these formals.

## Usage

``` r
multi_arm_designer(
  N = 90,
  m_arms = 3,
  outcome_means = rep(0, m_arms),
  sd_i = 1,
  outcome_sds = rep(0, m_arms),
  conditions = seq_len(m_arms),
  Y = NULL,
  args_to_fix = NULL
)
```

## Arguments

- N:

  Sample size.

- m_arms:

  Number of arms. May vary. Must match `length(outcome_means)` unless
  `outcome_means` is a scalar (then recycled).

- outcome_means:

  Average outcome in each arm. Default `rep(0, m_arms)`.

- sd_i:

  Standard deviation of the individual-level shock.

- outcome_sds:

  Extra standard deviation in each arm. Default `rep(0, m_arms)`.

- conditions:

  Assignment conditions, length `m_arms`. Default `seq_len(m_arms)`.

- Y:

  Optional outcome function of `Z`, `u`, and `outcome_sds`. The library
  default is used when `Y` is `NULL`.

- args_to_fix:

  Ignored. Present for compatibility with DesignLibrary 0.1.

## Value

A design object.

## See also

[`make_design()`](https://declaredesign.org/r/designlibrary/reference/make_design.md)

## Examples

``` r
make_design("multiarm_trial", N = 90)
#> Research design with 5 steps
#> 
#> Step 1 (model): declare_model(N = N, u = rnorm(N) * sd_i)
#> Step 2 (inquiry): declare_inquiry(handler = function(data, m_arms, outcome_means) { pairs <- combn(m_arms, 2) data.frame(inquiry = paste0("ate_Y_", pairs[2, ], "_", pairs[1, ]), estimand = as.numeric(outcome_means[pairs[2, ]] - outcome_means[pairs[1, ]])) }, m_arms = m_arms, outcome_means = per_arm(outcome_means, m_arms))
#> Step 3 (assignment): declare_assignment(Z = complete_ra(N, conditions = per_arm(conditions, m_arms)))
#> Step 4 (measurement): declare_measurement(Y = Y(Z, u, per_arm(outcome_sds, m_arms), per_arm(outcome_means, m_arms)))
#> Step 5 (estimator): declare_estimator(handler = function(data, m_arms, conditions) { pairs <- combn(m_arms, 2) estimates <- lapply(seq_len(ncol(pairs)), function(i) { k <- pairs[1, i] j <- pairs[2, i] fit <- difference_in_means(Y ~ Z, data = data, condition1 = conditions[k], condition2 = conditions[j]) cbind(estimator = paste0("DIM (Z_", j, " - Z_", k, ")"), tidy(fit), inquiry = paste0("ate_Y_", j, "_", k)) }) do.call(rbind, estimates) }, m_arms = m_arms, conditions = per_arm(conditions, m_arms))
#> 
#> Parameters and objects the design refers to:
#>           name                                    value     kind declared
#>     conditions                                      1:3   vector    FALSE
#>         m_arms                                        3   scalar    FALSE
#>              N                                       90   scalar    FALSE
#>  outcome_means                               c(0, 0, 0)   vector    FALSE
#>    outcome_sds                               c(0, 0, 0)   vector    FALSE
#>        per_arm function (x, m_arms) { k <- length(x)... function    FALSE
#>           sd_i                                        1   scalar    FALSE
#>              Y function (Z, u, outcome_sds, outcome_... function    FALSE
#>              ~ function (...) { .External2(ffi_tilde... function    FALSE
#>       steps
#>        3, 5
#>  2, 3, 4, 5
#>           1
#>        2, 4
#>           4
#>  2, 3, 4, 5
#>           1
#>        4, 5
#>           5
multi_arm_designer(m_arms = 4)
#> Research design with 5 steps
#> 
#> Step 1 (model): declare_model(N = N, u = rnorm(N) * sd_i)
#> Step 2 (inquiry): declare_inquiry(handler = function(data, m_arms, outcome_means) { pairs <- combn(m_arms, 2) data.frame(inquiry = paste0("ate_Y_", pairs[2, ], "_", pairs[1, ]), estimand = as.numeric(outcome_means[pairs[2, ]] - outcome_means[pairs[1, ]])) }, m_arms = m_arms, outcome_means = per_arm(outcome_means, m_arms))
#> Step 3 (assignment): declare_assignment(Z = complete_ra(N, conditions = per_arm(conditions, m_arms)))
#> Step 4 (measurement): declare_measurement(Y = Y(Z, u, per_arm(outcome_sds, m_arms), per_arm(outcome_means, m_arms)))
#> Step 5 (estimator): declare_estimator(handler = function(data, m_arms, conditions) { pairs <- combn(m_arms, 2) estimates <- lapply(seq_len(ncol(pairs)), function(i) { k <- pairs[1, i] j <- pairs[2, i] fit <- difference_in_means(Y ~ Z, data = data, condition1 = conditions[k], condition2 = conditions[j]) cbind(estimator = paste0("DIM (Z_", j, " - Z_", k, ")"), tidy(fit), inquiry = paste0("ate_Y_", j, "_", k)) }) do.call(rbind, estimates) }, m_arms = m_arms, conditions = per_arm(conditions, m_arms))
#> 
#> Parameters and objects the design refers to:
#>           name                                    value     kind declared
#>     conditions                                      1:4   vector    FALSE
#>         m_arms                                       4L   scalar    FALSE
#>              N                                       90   scalar    FALSE
#>  outcome_means                            c(0, 0, 0, 0)   vector    FALSE
#>    outcome_sds                            c(0, 0, 0, 0)   vector    FALSE
#>        per_arm function (x, m_arms) { k <- length(x)... function    FALSE
#>           sd_i                                        1   scalar    FALSE
#>              Y function (Z, u, outcome_sds, outcome_... function    FALSE
#>              ~ function (...) { .External2(ffi_tilde... function    FALSE
#>       steps
#>        3, 5
#>  2, 3, 4, 5
#>           1
#>        2, 4
#>           4
#>  2, 3, 4, 5
#>           1
#>        4, 5
#>           5
multi_arm_designer(m_arms = 4, outcome_means = c(0, 0.5, 1, 2))
#> Research design with 5 steps
#> 
#> Step 1 (model): declare_model(N = N, u = rnorm(N) * sd_i)
#> Step 2 (inquiry): declare_inquiry(handler = function(data, m_arms, outcome_means) { pairs <- combn(m_arms, 2) data.frame(inquiry = paste0("ate_Y_", pairs[2, ], "_", pairs[1, ]), estimand = as.numeric(outcome_means[pairs[2, ]] - outcome_means[pairs[1, ]])) }, m_arms = m_arms, outcome_means = per_arm(outcome_means, m_arms))
#> Step 3 (assignment): declare_assignment(Z = complete_ra(N, conditions = per_arm(conditions, m_arms)))
#> Step 4 (measurement): declare_measurement(Y = Y(Z, u, per_arm(outcome_sds, m_arms), per_arm(outcome_means, m_arms)))
#> Step 5 (estimator): declare_estimator(handler = function(data, m_arms, conditions) { pairs <- combn(m_arms, 2) estimates <- lapply(seq_len(ncol(pairs)), function(i) { k <- pairs[1, i] j <- pairs[2, i] fit <- difference_in_means(Y ~ Z, data = data, condition1 = conditions[k], condition2 = conditions[j]) cbind(estimator = paste0("DIM (Z_", j, " - Z_", k, ")"), tidy(fit), inquiry = paste0("ate_Y_", j, "_", k)) }) do.call(rbind, estimates) }, m_arms = m_arms, conditions = per_arm(conditions, m_arms))
#> 
#> Parameters and objects the design refers to:
#>           name                                    value     kind declared
#>     conditions                                      1:4   vector    FALSE
#>         m_arms                                       4L   scalar    FALSE
#>              N                                       90   scalar    FALSE
#>  outcome_means                          c(0, 0.5, 1, 2)   vector    FALSE
#>    outcome_sds                            c(0, 0, 0, 0)   vector    FALSE
#>        per_arm function (x, m_arms) { k <- length(x)... function    FALSE
#>           sd_i                                        1   scalar    FALSE
#>              Y function (Z, u, outcome_sds, outcome_... function    FALSE
#>              ~ function (...) { .External2(ffi_tilde... function    FALSE
#>       steps
#>        3, 5
#>  2, 3, 4, 5
#>           1
#>        2, 4
#>           4
#>  2, 3, 4, 5
#>           1
#>        4, 5
#>           5
```
