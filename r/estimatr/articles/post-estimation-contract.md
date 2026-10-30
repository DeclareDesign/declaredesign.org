# The post-estimation contract

A fitted `lm_robust` object is handed to many functions estimatr did not
write: [`coef()`](https://rdrr.io/r/stats/coef.html),
[`predict()`](https://rdrr.io/r/stats/predict.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`anova()`](https://rdrr.io/r/stats/anova.html),
[`hatvalues()`](https://rdrr.io/r/stats/influence.measures.html),
[`sandwich::vcovHC()`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html),
and dozens more. Some of these have estimatr methods, some fall through
to a method written for `lm`, and some have no business running on a fit
that does not store residuals or a QR decomposition. The post-estimation
contract records, for every one of 71 probes on each of 10 fits, what
estimatr promises the call will do. The table below is rendered from
`inst/contract/contract.csv`, the same file the test suite reads, so the
page cannot drift from what is checked.

## The promises

Each cell carries exactly one promise:

| code | promise | meaning |
|----|----|----|
| `=` | agrees | returns the same answer as the call on the reference fit ([`lm()`](https://rdrr.io/r/stats/lm.html), or [`ivreg::ivreg()`](https://zeileis.github.io/ivreg/reference/ivreg.html) for the IV fits), or on a stated reference |
| `R` | refuses | stops with estimatr’s own message saying the generic is not available for this fit |
| `E` | errors | stops, rather than return something that would mislead |
| `~` | returns | returns a value, with no reference answer to hold it to |
| `!` | known_wrong | a recorded gap; the test fails if it starts agreeing, so a fix is entered here |

Across the 710 cells: 326 agree, 90 refuse, 262 error, 32 return, 0 are
known wrong, and 0 make no promise.

The test (`tests/testthat/test_contract.R`) runs each probe in a fresh R
process holding estimatr and only the packages that probe needs, and
runs the reference in a process that never loads estimatr. Isolation
matters here: `clubSandwich` replaces estimatr’s
[`bread()`](https://zeileis.codeberg.page/sandwich/reference/bread.html)
and [`model.frame()`](https://rdrr.io/r/stats/model.frame.html) methods
when it is loaded second, and `performance` registers a
[`logLik()`](https://rdrr.io/r/stats/logLik.html) method for
`iv_robust`, so a probe run beside them measures them rather than
estimatr.

## What decides a cell

One rule assigns every promise, so that the next generic is placed by it
rather than by taste.

- A cell **agrees** wherever [`lm()`](https://rdrr.io/r/stats/lm.html)
  or
  [`ivreg::ivreg()`](https://zeileis.github.io/ivreg/reference/ivreg.html)
  answers the same call and the fit stores what the answer needs. Where
  estimatr has a method, it returns that answer; where it has none, the
  fall-through must already be right.
- A cell **refuses** where the reference answers but the fit cannot, and
  the message names what the fit lacks:
  [`difference_in_means()`](https://declaredesign.org/r/estimatr/reference/difference_in_means.md)
  and
  [`horvitz_thompson()`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md)
  store no residuals and no design, a `fixed_effects` fit does not store
  its absorbed design, a fit with several outcomes has one residual
  matrix for them all, and case-deletion diagnostics need leave-one-out
  refits that are not computed. A refusal replaces a `NULL`, a wrong
  number, or an unhelpful error with a sentence; it never replaces a
  working answer.
- A cell **errors** where no package should answer.
  [`logLik()`](https://rdrr.io/r/stats/logLik.html),
  [`AIC()`](https://rdrr.io/r/stats/AIC.html), and
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) are the case to know: the
  estimators assume no distribution, and estimatr registers no
  [`logLik()`](https://rdrr.io/r/stats/logLik.html) on any class,
  because performance registers one for
  [`iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.md)
  fits and a refusal on
  [`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.md)
  fits alone would make the answer depend on which package loaded last.
  The rest are generics that read internals an estimatr fit does not
  have, such as [`qr()`](https://rdrr.io/r/base/qr.html),
  [`effects()`](https://rdrr.io/r/stats/effects.html),
  [`alias()`](https://rdrr.io/r/stats/alias.html), and
  [`anova()`](https://rdrr.io/r/stats/anova.html).
- A cell **returns** only on
  [`horvitz_thompson()`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md),
  which has no reference fit.

## The fits

All ten are fit to one simulated dataset of 60 rows with a binary
treatment `z`, a covariate `x`, a binary instrument `iv`, ten clusters
`g`, a four-level factor `f`, weights `w` (three of them zero), and a
second outcome `y2`.

| fit | estimatr call | reference |
|----|----|----|
| `lm_plain` | `lm_robust(y ~ x + z, d)` | `lm(y ~ x + z, d)` |
| `lm_wt0` | `lm_robust(y ~ x + z, d, weights = w)` | `lm(y ~ x + z, d, weights = w)` |
| `lm_cl` | `lm_robust(y ~ x + z, d, clusters = g)` | `lm(y ~ x + z, d)` |
| `lm_fe` | `lm_robust(y ~ x + z, d, fixed_effects = ~f)` | `lm(y ~ x + z + f, d)` |
| `lm_mv` | `lm_robust(cbind(y, y2) ~ x + z, d)` | `lm(cbind(y, y2) ~ x + z, d)` |
| `lm_lin` | `lm_lin(y ~ z, ~x, d)` | `lm(y ~ z + x_c + z:x_c, d)` |
| `iv_plain` | `iv_robust(y ~ z + x \| iv + x, d)` | `ivreg::ivreg(y ~ z + x \| iv + x, data = d)` |
| `iv_cl` | `iv_robust(y ~ z + x \| iv + x, d, clusters = g)` | `ivreg::ivreg(y ~ z + x \| iv + x, data = d)` |
| `dim` | `difference_in_means(y ~ z, d)` | `lm(y ~ z, d)` |
| `ht` | `horvitz_thompson(y ~ z, d, condition_prs = ...)` | none |

The reference fits report classical variances, so a cell that concerns a
variance, a standard error, or an interval refers instead to the
variance estimatr reports by default:
`sandwich::vcovHC(ref, type = "HC2")`, or
`clubSandwich::vcovCR(ref, type = "CR2")` with Satterthwaite degrees of
freedom for the clustered fits. `lm_wt0`’s variances refer to the same
regression on its positive-weight rows, because `sandwich` counts
zero-weight rows as observations. `x_c` is `x` centered, as
[`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md)
centers it. `dim` and `ht` equal a regression on `z` only where a row
says so.

## The contract

| call | lm_plain | lm_wt0 | lm_cl | lm_fe | lm_mv | lm_lin | iv_plain | iv_cl | dim | ht |
|:---|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| `coef(fit)` | = | = | = | = | = | = | = | = | = | ~ |
| `coefficients(fit)` | = | = | = | = | = | = | = | = | = | ~ |
| `residuals(fit)` | = | = | = | = | = | = | = | = | R | ~ |
| `resid(fit)` | = | = | = | = | = | = | = | = | R | ~ |
| `fitted(fit)` | = | = | = | = | = | = | = | = | R | ~ |
| `fitted.values(fit)` | = | = | = | = | = | = | = | = | R | ~ |
| `predict(fit, newdata = d)` | = | = | = | = | = | = | = | = | E | E |
| `predict(fit, newdata = d, se.fit = TRUE)$se.fit` | = | = | = | R | R | = | = | = | E | E |
| `predict(fit)` | = | = | = | = | = | = | = | = | E | E |
| `confint(fit)` | = | = | = | = | = | = | = | = | = | ~ |
| `vcov(fit)` | = | = | = | = | = | = | = | = | = | ~ |
| `nobs(fit)` | = | = | = | = | = | = | = | = | = | ~ |
| `df.residual(fit)` | = | = | = | = | = | = | = | = | R | ~ |
| `deviance(fit)` | = | = | = | = | R | = | = | = | R | ~ |
| `as.numeric(logLik(fit))` | E | E | E | E | E | E | E | E | E | E |
| `AIC(fit)` | E | E | E | E | E | E | E | E | E | E |
| `BIC(fit)` | E | E | E | E | E | E | E | E | E | E |
| `extractAIC(fit)` | E | E | E | E | E | E | E | E | E | E |
| `sigma(fit)` | = | = | = | = | R | = | = | = | R | ~ |
| `format(formula(fit))` | = | = | = | = | = | = | = | = | = | ~ |
| `attr(terms(fit), "term.labels")` | = | = | = | = | = | ~ | = | = | E | E |
| `mf <- model.frame(fit); as.matrix(mf[vapply(mf, is.numeric, TRUE)])` | = | = | = | = | = | = | = | = | E | E |
| `model.matrix(fit)` | = | = | = | = | = | = | = | = | E | E |
| `model.matrix(fit, data = transform(d, x = x + 1, x_c = x_c + 1))` | = | = | = | = | = | = | = | = | E | E |
| `family(fit)$family` | E | E | E | E | E | E | E | E | E | E |
| `weights(fit)` | = | = | = | = | = | = | = | = | = | ~ |
| `case.names(fit)` | = | = | = | = | = | = | = | = | R | ~ |
| `variable.names(fit)` | = | = | = | = | = | = | = | = | R | ~ |
| `labels(fit)` | = | = | = | = | = | ~ | = | = | R | ~ |
| `unlist(dummy.coef(fit))` | E | E | E | E | E | E | E | E | E | E |
| `effects(fit)` | E | E | E | E | E | E | E | E | E | E |
| `alias(fit)$Complete` | E | E | E | E | E | E | E | E | E | E |
| `qr(fit)$rank` | E | E | E | E | E | E | E | E | E | E |
| `kappa(fit)` | E | E | E | E | E | E | E | E | E | E |
| `proj(fit)` | E | E | E | E | E | E | E | E | E | E |
| `hatvalues(fit)` | = | = | = | R | R | = | = | = | E | E |
| `cooks.distance(fit)` | R | R | R | R | R | R | R | R | E | E |
| `influence(fit)$hat` | R | R | R | R | R | R | R | R | E | E |
| `lm.influence(fit)$hat` | E | E | E | E | E | E | E | E | E | E |
| `influence.measures(fit)$infmat` | E | E | E | E | E | E | E | E | E | E |
| `dfbeta(fit)` | R | R | R | R | R | R | R | R | E | E |
| `dfbetas(fit)` | R | R | R | R | R | R | R | R | E | E |
| `rstandard(fit)` | R | R | R | R | R | R | R | R | E | E |
| `rstudent(fit)` | R | R | R | R | R | R | R | R | E | E |
| `add1(fit, ~. + x2, data = d)` | E | E | E | E | E | E | E | E | E | E |
| `drop1(fit)` | E | E | E | E | E | E | E | E | E | E |
| `formula(step(fit, trace = 0))` | E | E | E | E | E | E | E | E | E | E |
| `anova(fit)` | E | E | E | E | E | E | E | E | E | E |
| `anova(fit, update(fit, . ~ . - x))` | E | E | E | E | E | E | E | E | E | E |
| `coef(update(fit, . ~ . - x))` | = | = | = | = | = | = | = | = | = | ~ |
| `simulate(fit, 1)` | E | E | E | E | E | E | E | E | E | E |
| `plot(fit, which = 1)` | E | E | E | E | E | E | E | E | E | E |
| `coef(summary(fit))` | = | = | = | = | = | = | = | = | = | ~ |
| `summary(fit)$sigma` | = | = | = | = | = | = | = | = | = | ~ |
| `summary(fit)$r.squared` | = | = | = | = | = | = | = | = | = | ~ |
| `summary(fit)$fstatistic` | = | = | ~ | ~ | ~ | = | ~ | ~ | ~ | ~ |
| `fit$model` | = | = | = | = | = | = | = | = | = | ~ |
| `fit$na.action` | = | = | = | = | = | = | = | = | = | ~ |
| `sandwich::bread(fit)` | = | = | = | R | R | = | = | = | R | R |
| `sandwich::estfun(fit)` | = | = | = | R | R | = | = | = | E | E |
| `sandwich::meat(fit)` | = | = | = | R | R | = | = | = | E | E |
| `sandwich::sandwich(fit)` | = | = | = | R | R | = | = | = | E | E |
| `sandwich::vcovHC(fit, type = "HC2")` | = | = | = | R | R | = | = | = | E | E |
| `sandwich::vcovHC(fit, type = "HC3")` | = | = | = | R | R | = | = | = | E | E |
| `sandwich::vcovHC(fit, type = "HC0")` | = | = | = | R | R | = | = | = | E | E |
| `sandwich::vcovCL(fit, cluster = d$g)` | = | E | = | R | R | = | = | = | E | E |
| `sandwich::vcovCL(fit, cluster = d$g, type = "HC2")` | = | E | = | R | R | = | = | = | E | E |
| `sandwich::vcovHAC(fit)` | = | = | = | R | R | = | = | = | E | E |
| `sandwich::vcovBS(fit, R = 20)` | E | E | E | E | E | E | E | E | E | E |
| `sandwich::vcovOPG(fit)` | = | = | = | R | R | = | = | = | E | E |
| `bread(fit) %*% meat(fit) %*% bread(fit) / nobs(fit)` | = | = | = | R | R | = | = | = | E | E |

## Why each refusal

A refusal names what the fit lacks. These are the messages, and the
cells that carry them:

| message | cells | fits |
|:---|:---|:---|
| [`bread()`](https://zeileis.codeberg.page/sandwich/reference/bread.html) is not available | bread, sandwich | lm_fe, lm_mv |
| [`cooks.distance()`](https://rdrr.io/r/stats/influence.measures.html) is not available | cooks.distance | lm_plain, lm_wt0, lm_cl, lm_fe, lm_mv, lm_lin, iv_plain, iv_cl |
| [`deviance()`](https://rdrr.io/r/stats/deviance.html) is not available | deviance, sigma | lm_mv |
| [`dfbeta()`](https://rdrr.io/r/stats/influence.measures.html) is not available | dfbeta | lm_plain, lm_wt0, lm_cl, lm_fe, lm_mv, lm_lin, iv_plain, iv_cl |
| [`dfbetas()`](https://rdrr.io/r/stats/influence.measures.html) is not available | dfbetas | lm_plain, lm_wt0, lm_cl, lm_fe, lm_mv, lm_lin, iv_plain, iv_cl |
| [`estfun()`](https://zeileis.codeberg.page/sandwich/reference/estfun.html) is not available | estfun, meat, vcovHC, vcovCL, vcovHAC, vcovOPG | lm_fe, lm_mv |
| [`hatvalues()`](https://rdrr.io/r/stats/influence.measures.html) is not available | hatvalues | lm_fe, lm_mv |
| [`influence()`](https://rdrr.io/r/stats/lm.influence.html) is not available | influence | lm_plain, lm_wt0, lm_cl, lm_fe, lm_mv, lm_lin, iv_plain, iv_cl |
| [`rstandard()`](https://rdrr.io/r/stats/influence.measures.html) is not available | rstandard | lm_plain, lm_wt0, lm_cl, lm_fe, lm_mv, lm_lin, iv_plain, iv_cl |
| [`rstudent()`](https://rdrr.io/r/stats/influence.measures.html) is not available | rstudent | lm_plain, lm_wt0, lm_cl, lm_fe, lm_mv, lm_lin, iv_plain, iv_cl |
| Can’t set `se.fit` == TRUE with multivariate outcome | predict | lm_mv |
| Can’t set `se.fit` or `interval` with `fixed_effects` | predict | lm_fe |
| not built from a design matrix | bread | dim, ht |
| the estimator has no residuals | df.residual | dim |
| the estimator has no residuals or fitted values | residuals, resid, fitted, fitted.values, deviance, sigma | dim |
| the estimator is not built from a design matrix | variable.names, labels | dim |
| the rows used are not stored on the fit | case.names | dim |

## Decisions

Some cells could reasonably have gone either way: whether
[`df.residual()`](https://rdrr.io/r/stats/df.residual.html) reports N -
K or the cluster-adjusted degrees of freedom, whether
[`logLik()`](https://rdrr.io/r/stats/logLik.html) on a robust fit should
exist at all. Where a cell’s promise was a choice rather than a
consequence, the maintainer’s decision is recorded beside it:

| decision | promise | generics | fits |
|:---|:---|:---|:---|
| refused | refuses | residuals, resid, fitted, fitted.values, deviance, sigma, case.names, variable.names, labels | dim |
| multi-outcome refused | refuses | predict, deviance, sigma, hatvalues, bread, estfun, meat, sandwich, vcovHC, vcovCL, vcovHAC, vcovOPG | lm_mv |
| per outcome, as lm() reports them | agrees | confint, summary | lm_mv |
| N - K; consumers get upstream fixes | agrees | df.residual | lm_plain, lm_wt0, lm_cl, lm_fe, lm_mv, lm_lin, iv_plain, iv_cl |
| refused; the tests use fit\$df | refuses | df.residual | dim |
| N - K; consumers get upstream fixes | returns | df.residual | ht |
| implement with lm’s meaning | agrees | deviance, sigma | lm_plain, lm_wt0, lm_cl, lm_fe, lm_lin, iv_plain, iv_cl |
| no logLik() method on any class; performance registers one for iv_robust, so a refusal on lm_robust alone made AIC(fit) depend on load order | errors | logLik, AIC, BIC, extractAIC | lm_plain, lm_wt0, lm_cl, lm_fe, lm_mv, lm_lin, iv_plain, iv_cl |
| terms() full (lm_lin translated), formula() the call’s | agrees | formula | lm_fe, lm_lin |
| terms() full (lm_lin translated), formula() the call’s | agrees | terms, labels | lm_fe |
| terms() full (lm_lin translated), formula() the call’s | returns | terms, labels | lm_lin |
| the regressors, as ivreg’s terms() default to | agrees | terms | iv_plain, iv_cl |
| implement with lm’s meaning | agrees | case.names | lm_plain, lm_wt0, lm_cl, lm_fe, lm_lin |
| implement with lm’s meaning; the reference has no case.names() of its own | agrees | case.names | lm_mv, iv_plain, iv_cl |
| implement with lm’s meaning | agrees | labels | lm_plain, lm_wt0, lm_cl, iv_plain, iv_cl |
| implement with lm’s meaning | agrees | hatvalues | lm_plain, lm_wt0, lm_cl, lm_lin, iv_plain, iv_cl |
| implement with lm’s meaning | refuses | hatvalues | lm_fe |
| refuse case-deletion diagnostics | refuses | cooks.distance, influence, dfbeta, dfbetas, rstandard, rstudent | lm_plain, lm_wt0, lm_cl, lm_fe, lm_mv, lm_lin, iv_plain, iv_cl |
| refuse case-deletion diagnostics | errors | cooks.distance, influence, dfbeta, dfbetas, rstandard, rstudent | dim, ht |
| reaches extractAIC(), which has no method on any class | errors | add1 | lm_plain, lm_wt0, lm_cl, lm_mv |
| reaches extractAIC(), which has no method on any class | errors | drop1, step | lm_plain, lm_wt0, lm_cl, lm_fe, lm_mv, lm_lin |
| summary() carries sigma, as summary.lm() does | agrees | summary | lm_plain, lm_wt0, lm_cl, lm_fe, lm_lin, iv_plain, iv_cl |
| dim has no residual quantities; `$` cannot refuse, so NULL is the promise | agrees | summary | dim |
| not stored, as lm(model = FALSE); consumers fall back to model.frame() | agrees | \$model | lm_plain, lm_wt0, lm_cl, lm_fe, lm_mv, lm_lin, iv_plain, iv_cl, dim |
