# Random assignment with tight targets

**Experimental.** `balanced_ra` draws random assignment with tight
targets: condition counts at the floor or ceiling of what the
probabilities imply, and, with `formula`, covariate totals too. Each
unit's probability stays exact. That is useful when probabilities vary
across units, and also when they do not: leftover pairing keeps two-arm
blocked counts tight overall as well as within each block, and cube-on-X
balances a continuous covariate without binning it.

## Usage

``` r
balanced_ra(
  N = NULL,
  prob = NULL,
  prob_unit = NULL,
  prob_unit_each = NULL,
  blocks = NULL,
  clusters = NULL,
  num_arms = NULL,
  conditions = NULL,
  formula = NULL,
  check_inputs = TRUE,
  .X = NULL
)
```

## Arguments

- N:

  The number of units. Optional when `formula` or the length of
  `prob_unit` (or `blocks` or `clusters`) identifies N. A single
  positive integer. If supplied it must match. (optional)

- prob:

  A single number between 0 and 1: the probability of assignment to
  treatment, shared by every unit, for a two-arm design. Defaults to 0.5
  when no probability argument is supplied, so `balanced_ra(4)` is
  complete assignment of four units. Supply exactly one of `prob`,
  `prob_unit` and `prob_unit_each`. (optional)

- prob_unit:

  A numeric vector of length N giving each unit's probability of
  assignment to treatment, for a two-arm design. Unlike elsewhere in
  randomizr these need not be equal across units. A single number is
  refused, since that is what `prob` is for. Supply exactly one of
  `prob`, `prob_unit` and `prob_unit_each`. (optional)

- prob_unit_each:

  A numeric matrix with one row per unit and one column per condition,
  giving each unit's probability of assignment to each condition, for a
  multi-arm design. Rows must sum to 1. Supply exactly one of `prob`,
  `prob_unit` and `prob_unit_each`. (optional)

- blocks:

  A vector of length N indicating which block each unit belongs to. When
  supplied, two-arm counts are held tight within each block and overall;
  with three or more arms the tight counts are the within-block ones.
  (optional)

- clusters:

  A vector of length N indicating which cluster each unit belongs to.
  Whole clusters are assigned together, so the probabilities must be the
  same for every unit in a cluster, and the tight counts become counts
  of clusters rather than of units. May be combined with `blocks`, in
  which case every cluster must sit entirely inside one block. May also
  be combined with `formula`, in which case each cluster's covariates
  are the averages of its units' covariates, so that a cluster counts
  once however many units it holds and the treated count that is held
  tight remains a count of clusters. (optional)

- num_arms:

  The number of treatment arms. Inferred when omitted. Supplied without
  any probability argument, `num_arms` (or `conditions`) of three or
  more expands to equal-probability assignment, as in
  [`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md).
  (optional)

- conditions:

  A vector giving the names of the conditions. (optional)

- formula:

  A model formula whose model matrix is the balancing matrix \\X\\ in
  the cube method, e.g. `~ x + B`. The intercept column is the count
  constraint; `~ 0 + x` drops it and the treated count may wander. Names
  are looked up where the formula was written, then in the calling
  frame, so the usual `dat |> mutate(Z = balanced_ra(formula = ~ x))`
  finds the column `x`. Two-arm only. May be combined with `clusters`;
  cannot be combined with `blocks` or `prob_unit_each`. (optional)

- check_inputs:

  Logical. Whether to verify before assigning that the arguments are
  internally consistent: that probabilities lie between 0 and 1, that
  rows of a probability matrix sum to 1, that probabilities are constant
  within a cluster, and that clusters nest within blocks. Defaults to
  `TRUE`. Set to `FALSE` to skip the checks when drawing many
  assignments from probabilities that have already been verified.
  (optional)

- .X:

  Internal. A balancing matrix already built from `formula`, supplied by
  [`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
  so that the formula's variables are looked up once, when the design is
  declared, rather than on every draw. Not for direct use. (optional)

## Value

A vector of length N giving the condition of each unit. As in
[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md):
integer 0/1 in a two-arm design, unless `num_arms` or `conditions` is
supplied explicitly, in which case a factor ordered by `conditions`; a
factor in a multi-arm design.

## Details

With unit-varying probabilities it fills the gap between
[`simple_ra()`](https://declaredesign.org/r/randomizr/reference/simple_ra.md),
which honors those probabilities but lets the number treated wander, and
[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md),
which fixes the number treated but requires every unit to share the same
probability.

The "balanced" in the name is balanced sampling in the sense of Deville
and Tillé (2004). With the default arguments the realized counts are
held against their targets. Pass `formula` to add linear balancing
constraints on covariates (cube-on-X): the flight keeps \\X'Z\\ near
\\X'\pi\\. Landing may drop a constraint, so exact tightness on every
column is not always possible. `blocks` is a different device: it
tightens counts inside discrete groups. The two cannot be combined.

Two motivating cases: a race in which contestants have unequal chances
and exactly one must win; and two districts of three villages, three to
treat, blocked by district, so that each district should receive one or
two and the total should be three.

## What is guaranteed

Every unit receives exactly one condition. Each unit's probability of
each condition is the probability supplied. Counts are tight within each
block always, and tight overall as well when there are two arms. With
three or more arms and `blocks`, the overall count can wander; see the
vignette *Introduction to balanced_ra*. With `clusters`, the tight
counts are counts of clusters. With `formula`, first-order inclusion
probabilities remain exact; covariate totals are as close as the landing
phase allows. See that vignette.

Tight counts have one exception, and it is an arithmetic one rather than
a design one. Each step of the algorithm is sized so that at least one
unit lands exactly on 0 or on 1. Every so often rounding error in
floating-point arithmetic leaves every unit in that step a hair short of
its bound, and the function then settles the unit with the least room
left by a coin weighted by the value that unit currently holds. That
coin keeps the unit's assignment probability exactly right, so the
probability guarantee is untouched. It does not respect the count, so a
draw that reaches this fallback can finish one unit away from the floor
or the ceiling. We have not been able to make it happen: it did not
arise in any of several thousand draws across dozens of randomly
generated designs. It is documented because it is reachable in
principle, not because it is expected in practice.

## Balance when probabilities vary

The cube holds \\X'Z\\ near \\X'\pi\\, which is the treated total of
each balancing column against the total its assignment probabilities
imply. When every unit shares a probability, that target amounts to
splitting the column evenly between the arms, and `formula` does what
its name suggests. When probabilities vary from unit to unit, the two
targets come apart.

Suppose \\p_i\\ rises with \\x_i\\. High-\\x\\ units are meant to be
treated more often, so the treated group ought to have the higher mean
of \\x\\, and it does: the average treated-minus-control difference in
\\x\\ under `formula = ~ x` is the same one
[`simple_ra()`](https://declaredesign.org/r/randomizr/reference/simple_ra.md)
gives on the same probabilities. What the cube tightens is the spread of
that difference around its target, and with it the Horvitz-Thompson
residual for the \\x\\ total.

In short, `formula` does not equalize the arms when \\p_i\\ varies, and
it is not meant to. Weight by the reciprocal of the assignment
probability, as for any unequal-probability design;
[`balanced_ra_probabilities()`](https://declaredesign.org/r/randomizr/reference/balanced_ra_probabilities.md)
returns the probabilities to weight by. With a constant \\p\\ the
question does not arise.

## Order of the covariates

The flight phase sorts units by the first column of \\X\\ that is not
constant and works through them in a sliding window, so each step pairs
units with nearby values of that column. An intercept is a column of
ones and so is passed over, which makes the sort column `x` under `~ x`
and `x1` under `~ 0 + x1 + x2`. The design therefore balances smooth
functions of that first covariate and not only its linear total: in
simulations at \\N = 200\\ with a constant \\p\\, the
treated-minus-control spread in \\x^2\\ and \\x^3\\ runs several times
tighter than under
[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md),
though how much tighter varies with the covariate draw, and a
heavy-tailed \\x\\ narrows the gain.

The gain is also uneven. Only one column drives the sort, so under
`~ x1 + x2` the spread in \\x_1^2\\ tightens while the spread in
\\x_2^2\\ stays about where complete assignment leaves it. Both linear
totals are held tight. A covariate you name but do not put first is
balanced in its own right and in nothing else, and a covariate you do
not name at all is not balanced.

Sorting is a choice made here rather than a feature of the cube method,
which constrains only the linear span of \\X\\. Put the covariate whose
relationship with the outcome you least trust yourself to model first in
the formula.

## Analyzing the result

When \\p_i\\ varies across units, an unweighted comparison of means is
not the average treatment effect. Weight each unit by the reciprocal of
the probability of the condition it landed in;
[`balanced_ra_probabilities()`](https://declaredesign.org/r/randomizr/reference/balanced_ra_probabilities.md)
returns the matrix of probabilities those weights are built from, in the
same form as the other `_probabilities` functions in randomizr.

Standard errors then divide into two cases, and the vignette
*Introduction to balanced_ra* measures both.

On the count-tight designs, meaning every call that does not pass
`formula`, the usual heteroskedasticity-consistent intervals behave
about as they do after
[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md).
Holding counts tight makes assignments negatively dependent across
units, which is a reason to ask the question, but in simulation it did
not move HC2 coverage appreciably away from its nominal rate for
two-arm, blocked two-arm or three-arm designs.

With `formula` it is different. The design removes assignment variance
that the variance estimator cannot see, so the reported interval is
wider than the estimator's true sampling variability warrants. At \\N =
200\\ with a strongly prognostic \\x\\, HC2 on an unadjusted regression
covered the true effect on every draw, with an average standard error
well over twice the estimator's actual standard deviation. That is valid
but wasteful: it discards the precision the design was chosen to buy.
Fitting Lin's estimator on the same columns recovers most of it, and
stops recovering it when the adjustment model is wrong, so the case for
this design is strongest exactly where the reported interval understates
the gain. Adjusting linearly for \\x\\ when the outcome was quadratic in
it, for instance, returned coverage to 1.000 with the standard error
again more than twice too large.

[`estimatr::horvitz_thompson()`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.html)
is conservative here for a related reason, and an exact variance is not
a missing feature so much as an open problem: the joint inclusion
probabilities of a cube design have no closed form. That is what Deville
and Tillé (2005) approximate, and randomizr does not implement that
approximation.

## Experimental

This function is new in randomizr 2.0.1 and its interface may change.
Declare a design with
[`declare_ra()`](https://declaredesign.org/r/randomizr/reference/declare_ra.md)
by setting `ra_type = "balanced"` or by supplying `prob_unit_each` or
`formula`;
[`conduct_ra()`](https://declaredesign.org/r/randomizr/reference/conduct_ra.md)
and
[`obtain_condition_probabilities()`](https://declaredesign.org/r/randomizr/reference/obtain_condition_probabilities.md)
then dispatch here. The vignette *Introduction to balanced_ra* has the
count-tight algorithm and a four-unit cube-on-X walk-through.

## References

Deville, J.-C. and Tillé, Y. (2004). Efficient balanced sampling: the
cube method. *Biometrika* 91(4), 893-912.
[doi:10.1093/biomet/91.4.893](https://doi.org/10.1093/biomet/91.4.893)

Deville, J.-C. and Tillé, Y. (1998). Unequal probability sampling
without replacement through a splitting method. *Biometrika* 85(1),
89-101.
[doi:10.1093/biomet/85.1.89](https://doi.org/10.1093/biomet/85.1.89)

Chauvet, G. and Tillé, Y. (2006). A fast algorithm for balanced
sampling. *Computational Statistics* 21(1), 53-62.
[doi:10.1007/s00180-006-0250-2](https://doi.org/10.1007/s00180-006-0250-2)

Deville, J.-C. and Tillé, Y. (2005). Variance approximation under
balanced sampling. *Journal of Statistical Planning and Inference*
128(2), 569-591.
[doi:10.1016/j.jspi.2003.11.011](https://doi.org/10.1016/j.jspi.2003.11.011)

## See also

[`balanced_ra_probabilities()`](https://declaredesign.org/r/randomizr/reference/balanced_ra_probabilities.md),
[`complete_ra()`](https://declaredesign.org/r/randomizr/reference/complete_ra.md),
[`block_ra()`](https://declaredesign.org/r/randomizr/reference/block_ra.md),
[`simple_ra()`](https://declaredesign.org/r/randomizr/reference/simple_ra.md),
the vignette *Introduction to balanced_ra*

## Examples

``` r
# Four units, default probability 0.5: complete assignment of two treated.
table(balanced_ra(4))
#> 
#> 0 1 
#> 2 2 

# A race between contestants with unequal chances, in which exactly one wins
# because the chances sum to 1.
chances <- c(0.5, 0.3, 0.15, 0.05)
winners <- replicate(1000, which(balanced_ra(prob_unit = chances) == 1))
table(winners) / 1000     # close to chances
#> winners
#>     1     2     3     4 
#> 0.502 0.295 0.152 0.051 

# Unequal probabilities, two arms, with the number treated held tight.
p <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
Z <- balanced_ra(prob_unit = p)
table(Z)
#> Z
#> 0 1 
#> 3 3 

# Repeating the draw: probabilities are honored, and exactly 3 are treated
# every time because the probabilities sum to 3.
reps <- replicate(1000, balanced_ra(prob_unit = p))
rowMeans(reps)          # close to p
#> [1] 0.226 0.389 0.599 0.811 0.475 0.500
table(colSums(reps))    # always 3
#> 
#>    3 
#> 1000 

# Two districts of three villages, three to be treated, blocked by district.
# Each district gets one or two; the total is always three.
districts <- rep(c("north", "south"), each = 3)
reps <- replicate(1000, balanced_ra(blocks = districts))
table(colSums(reps))                           # always 3
#> 
#>    3 
#> 1000 
table(colSums(reps[districts == "north", ]))   # 1 or 2
#> 
#>   1   2 
#> 524 476 

# Three arms with unit-varying probabilities.
P <- cbind(c(0.15, 0.47), c(0.65, 0.48), c(0.20, 0.05))
table(replicate(1000, balanced_ra(prob_unit_each = P))[1, ])
#> 
#>  T1  T2  T3 
#> 144 665 191 

# Whole clusters assigned together, with unequal cluster probabilities. The
# number of treated clusters is fixed; the number of treated units is not,
# because the clusters differ in size.
clusters <- rep(1:6, times = c(3, 1, 4, 2, 5, 3))
p_cluster <- c(0.2, 0.4, 0.6, 0.8, 0.5, 0.5)
Z <- balanced_ra(prob_unit = p_cluster[clusters], clusters = clusters)
table(clusters, Z)
#>         Z
#> clusters 0 1
#>        1 3 0
#>        2 1 0
#>        3 4 0
#>        4 0 2
#>        5 0 5
#>        6 0 3

# Blocks and clusters together: a tight number of treated clusters in each
# block.
blocks <- ifelse(clusters <= 3, "east", "west")
Z <- balanced_ra(prob = 0.5, clusters = clusters, blocks = blocks)
table(blocks, Z)
#>       Z
#> blocks 0 1
#>   east 4 4
#>   west 3 7

# Cube-on-X: keep the treated total of a continuous covariate near its
# target. The intercept in ~ x is the count constraint. N is inferred
# from the looked-up formula variables.
x <- c(1, 2, 3, 6)
Z <- balanced_ra(formula = ~ x)
sum(x * Z)   # near 6
#> [1] 4

# Cube-on-X with clusters. Each cluster is treated as one unit carrying the
# average of its members' covariates, so three of the six clusters are
# treated on every draw and it is the cluster means of x that are balanced.
x_cl <- c(-2, -1, 0, 1, 2, 3)[clusters]
Z <- balanced_ra(prob = 0.5, clusters = clusters, formula = ~ x_cl)
table(clusters, Z)
#>         Z
#> clusters 0 1
#>        1 0 3
#>        2 1 0
#>        3 4 0
#>        4 2 0
#>        5 0 5
#>        6 0 3
```
