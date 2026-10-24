# Package index

## Fabricating data

The entry point. Everything else is called from inside it.

- [`fabricate()`](https://declaredesign.org/r/fabricatr/reference/fabricate.md)
  : Fabricate a data frame

## Levels

Give the data a shape: units inside clusters, a panel of countries and
years, or a second population linked to the first.

- [`add_level()`](https://declaredesign.org/r/fabricatr/reference/add_level.md)
  : Add a level, nested in the one before it if there is one
- [`nest_level()`](https://declaredesign.org/r/fabricatr/reference/nest_level.md)
  : Nest a level in the one before it (same as a later add_level())
- [`declare_level()`](https://declaredesign.org/r/fabricatr/reference/declare_level.md)
  : Declare an independent level for cross-classification
- [`import_level()`](https://declaredesign.org/r/fabricatr/reference/import_level.md)
  : Import an existing data frame as a level
- [`cross_levels()`](https://declaredesign.org/r/fabricatr/reference/cross_levels.md)
  : Create a full Cartesian product of declared levels
- [`link_levels()`](https://declaredesign.org/r/fabricatr/reference/link_levels.md)
  : Sample N rows from a Cartesian product with optional correlation
- [`modify_level()`](https://declaredesign.org/r/fabricatr/reference/modify_level.md)
  : Modify columns of an existing level, or of the rows in hand

## Potential outcomes

An outcome under each condition, and the one a unit actually shows.

- [`potential_outcomes()`](https://declaredesign.org/r/fabricatr/reference/potential_outcomes.md)
  : Build potential outcomes columns
- [`reveal_outcomes()`](https://declaredesign.org/r/fabricatr/reference/reveal_outcomes.md)
  : Reveal observed outcomes from potential outcomes columns

## Drawing discrete variables

Turn a latent variable into the kind of measurement a survey returns.

- [`draw_binary()`](https://declaredesign.org/r/fabricatr/reference/draw_binary.md)
  : Draw binary (0/1) outcomes
- [`draw_binomial()`](https://declaredesign.org/r/fabricatr/reference/draw_binomial.md)
  : Draw binomial counts
- [`draw_count()`](https://declaredesign.org/r/fabricatr/reference/draw_count.md)
  : Draw Poisson count data
- [`draw_categorical()`](https://declaredesign.org/r/fabricatr/reference/draw_categorical.md)
  : Draw categorical outcomes
- [`draw_ordered()`](https://declaredesign.org/r/fabricatr/reference/draw_ordered.md)
  : Draw ordered categorical outcomes from a latent variable
- [`draw_likert()`](https://declaredesign.org/r/fabricatr/reference/draw_likert.md)
  : Recode a latent variable into a Likert response
- [`split_quantile()`](https://declaredesign.org/r/fabricatr/reference/split_quantile.md)
  : Split data into quantile buckets
- [`draw_quantile()`](https://declaredesign.org/r/fabricatr/reference/draw_quantile.md)
  : Draw quantile bucket assignments

## Drawing clustered variables

Draws with a target intra-cluster correlation, or serial correlation
within a unit over time.

- [`draw_normal_icc()`](https://declaredesign.org/r/fabricatr/reference/draw_normal_icc.md)
  : Draw normally distributed data with a target intra-cluster
  correlation
- [`draw_binary_icc()`](https://declaredesign.org/r/fabricatr/reference/draw_binary_icc.md)
  : Draw binary data with a target intra-cluster correlation
- [`draw_normal_ar()`](https://declaredesign.org/r/fabricatr/reference/draw_normal_ar.md)
  : Draw normally distributed data with serial correlation within
  clusters

## Drawing correlated variables

Several variables at once, or one correlated with a variable you have.

- [`draw_multivariate()`](https://declaredesign.org/r/fabricatr/reference/draw_multivariate.md)
  : Draw multivariate random variables
- [`correlate()`](https://declaredesign.org/r/fabricatr/reference/correlate.md)
  : Generate a variable correlated with an existing variable

## Resampling

Bootstrap a data frame, respecting the levels it was built with.

- [`resample_data()`](https://declaredesign.org/r/fabricatr/reference/resample_data.md)
  : Resample a data frame, with optional hierarchical cluster resampling
- [`ALL`](https://declaredesign.org/r/fabricatr/reference/ALL.md) : Keep
  every unit at a resampling level

## Calling fabricatr from a package

For a caller that captures the expressions itself, as DeclareDesign’s
declaration verbs do.

- [`fabricate_with_dots()`](https://declaredesign.org/r/fabricatr/reference/fabricate_with_dots.md)
  : Fabricate from a list of quosures

## fabricatr 1.x spellings

Kept so that scripts written for fabricatr 1.x run unchanged. Each is
accepted and warns once with the call to write instead.

- [`join_using()`](https://declaredesign.org/r/fabricatr/reference/join_using.md)
  : Name the levels to cross or link
- [`recycle()`](https://declaredesign.org/r/fabricatr/reference/recycle.md)
  : Recycle a vector to the length of the level being built
