# Defunct step declarations from DeclareDesign 1.x

`declare_potential_outcomes()` and `declare_reveal()` were separate
steps in 1.x. In 2.0 potential outcomes belong in
[`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md)
and revealing belongs in
[`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md),
so neither has a 2.0 equivalent as a step of its own.

## Usage

``` r
declare_potential_outcomes(...)

declare_reveal(...)
```

## Arguments

- ...:

  (optional) Ignored.

## Value

Never returns; always raises an error.

## Details

They exist here only so that 1.x code fails with a message saying what
to write instead. Without them R reports `could not find function`,
which says nothing about the replacement.
