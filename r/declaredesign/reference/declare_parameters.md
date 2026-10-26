# Declare the parameters of a design

Names the quantities a design is built from, so that
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
changes them and nothing else. A parameter declared here is bound for
every step that follows, and takes precedence over a binding of the same
name in the workspace.

Parameters are ordinary R expressions evaluated once, when the design is
built, and each may read the ones declared before it:
`declare_parameters(m_arms = 3, ks = seq_len(m_arms)[-1])`. They are not
redrawn on each simulation, which is what separates a parameter from a
random variable: `declare_parameters(u = rnorm(1))` fixes one number for
the life of the design.

## Usage

``` r
declare_parameters(..., label = "parameters")
```

## Arguments

- ...:

  (optional) Named parameters. Each is evaluated once, in order, and may
  read the parameters declared before it.

- label:

  (optional) Step label. Defaults to `"parameters"`.

## Value

A `design_step` (class `c("design_step", "dd", "function")`) that
carries the declared parameters and their values. Called on a data
frame, it returns the data unchanged. Add it to a design with `+`.

## Details

Without a `declare_parameters()` step, a design's parameters are
whatever its expressions read out of the environment they were written
in, and
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
finds them by name. That works and it stays supported:
`declare_model(N = n_units, ...)` with `n_units` defined outside the
design needs no declaration.

What does need a name is a number written into the design.
`declare_model(N = 500)` puts 500 in the design, and nothing outside it
names that number, so `redesign(design, N = 1000)` errors rather than
rewriting the argument because its name matched. Giving the value a name
outside the design fixes that on its own, and is the usual answer;
declaring it here earns its place when several steps read the value, or
when a column shares its name. A redesign changes the declared
parameter, and a column of the same name is left alone, because it
belongs to the data rather than to the design.

The step generates no data and is skipped when the design runs.

## See also

Other design declarations:
[`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md),
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md),
[`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md),
[`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md),
[`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md),
[`declare_notes()`](https://declaredesign.org/r/declaredesign/reference/declare_notes.md),
[`declare_sampling()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md),
[`declare_step()`](https://declaredesign.org/r/declaredesign/reference/declare_step.md),
[`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)

## Examples

``` r
design <-
  declare_parameters(n_units = 100, effect = 0.5) +
  declare_model(N = n_units, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + effect) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = sample(rep(0:1, length.out = N))) +
  declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z)

nrow(draw_data(design))
#> [1] 100
nrow(draw_data(redesign(design, n_units = 40)))
#> [1] 40

# A later parameter may read an earlier one.
design <- declare_parameters(m_arms = 3, ks = seq_len(m_arms)[-1]) +
  declare_model(N = 60) +
  declare_assignment(Z = complete_ra(N, conditions = seq_len(m_arms)))
design_parameters(design)
#>    name value   kind declared steps
#>      ks   2:3 vector     TRUE     1
#>  m_arms     3 scalar     TRUE  1, 3
```
