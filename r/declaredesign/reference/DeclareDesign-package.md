# DeclareDesign: A Tidyverse-First Research Design Declaration and Diagnosis Tool

Declares, simulates, diagnoses and redesigns research designs. A design
is declared as a model, an inquiry, a data strategy and an answer
strategy, then simulated with purrr and diagnosed with dplyr. Version
2.0 is a ground-up rewrite of the package on tidyverse conventions.

## Details

A design is a sequence of steps combined with `+`. Each step is one of
the four elements of a research design:

- [`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md)
  describes the world the design runs in.

- [`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md)
  records the question. Its value on a draw is the estimand.

- [`declare_sampling()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md),
  [`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md),
  and
  [`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md)
  make up the data strategy: who is observed, what they are assigned,
  and what is measured.

- [`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
  and
  [`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)
  are the answer strategy.

[`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md)
simulates a design many times and reports how its estimates compare to
its estimands: bias, power, coverage, and the rest.
[`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
changes a parameter and returns a new design, so two versions can be
compared before any data are collected.

    design <-
      declare_model(N = 100, U = rnorm(N), Y_Z_0 = U, Y_Z_1 = U + 0.5) +
      declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
      declare_assignment(Z = sample(rep(0:1, length.out = N))) +
      declare_measurement(Y = Y_Z_0 * (1 - Z) + Y_Z_1 * Z) +
      declare_estimator(Y ~ Z, .method = lm, term = "Z", inquiry = "ATE")

    diagnose_design(design)

## See also

Useful links:

- <https://declaredesign.org/r/declaredesign/>

- <https://github.com/DeclareDesign/DeclareDesign>

- Report bugs at <https://github.com/DeclareDesign/DeclareDesign/issues>

## Author

**Maintainer**: Alexander Coppock <acoppock@gmail.com>
([ORCID](https://orcid.org/0000-0002-5733-2386))

Authors:

- Alexander Coppock <acoppock@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-5733-2386))

- Graeme Blair <graeme.blair@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-9164-2102))

- Jasper Cooper <jaspercooper@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-8639-3188))

- Macartan Humphreys <macartan@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-7029-2326))

- Neal Fultz <nfultz@gmail.com>
