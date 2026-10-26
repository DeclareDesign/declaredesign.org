# Defunct comparison helpers

These functions existed in DeclareDesign 1.x but are not part of 2.0.
Each compared two designs by printing their code, their draws, or their
summaries side by side, which is a job for a diff of the two scripts.
They error with a message saying so.

## Usage

``` r
compare_designs(...)

compare_design_code(...)

compare_design_data(...)

compare_design_estimates(...)

compare_design_inquiries(...)

compare_design_summaries(...)

print_code(...)
```

## Arguments

- ...:

  (optional) Ignored.

## Value

Never returns; always raises an error.

## Details

[`compare_diagnoses()`](https://declaredesign.org/r/declaredesign/reference/compare_diagnoses.md),
which compares what two designs achieve rather than how they are
written, is implemented.
