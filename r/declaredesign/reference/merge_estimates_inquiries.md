# Join estimates to their inquiries

Used in
[`run_design()`](https://declaredesign.org/r/declaredesign/reference/run_design.md)
and
[`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)
to attach estimands to estimator output via the `inquiry` label.

## Usage

``` r
merge_estimates_inquiries(estimates, inquiries)
```

## Arguments

- estimates:

  A tibble of estimator output.

- inquiries:

  A tibble of inquiry output.

## Value

A tibble, carrying a `matched_on` attribute.

## Details

The join is on every column the two tables share, so a labelled
estimator matches on `inquiry`, a conjoint matches on the `attribute`
and `level` columns both of its tables carry, and an estimator declared
without `inquiry =` still matches on `sim_ID` alone. That last case is
what makes the one-inquiry, one-estimator design work without the label:
the simulation's only inquiry is the one attached.

When the join produces more rows than either table had, the match was
genuinely ambiguous and a warning names the columns it went on.

The columns the join went on are recorded on the result as the
`matched_on` attribute, which
[`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)
carries to the simulations table and
[`print.diagnosis()`](https://declaredesign.org/r/declaredesign/reference/print.diagnosis.md)
reports when the match was anything other than a labelled one.
