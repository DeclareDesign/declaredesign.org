# Package index

## Declare

The steps of a design, joined with `+`.

- [`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md)
  : Declare the data-generating model
- [`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md)
  [`declare_inquiries()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md)
  [`declare_estimand()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md)
  [`declare_estimands()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md)
  : Declare an inquiry
- [`declare_sampling()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md)
  : Declare a sampling procedure
- [`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md)
  : Declare an assignment procedure
- [`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md)
  : Declare measurement
- [`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
  : Declare an estimator
- [`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)
  : Declare a hypothesis test
- [`declare_step()`](https://declaredesign.org/r/declaredesign/reference/declare_step.md)
  : Declare a custom data-handling step
- [`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md)
  : Declare the parameters of a design
- [`declare_notes()`](https://declaredesign.org/r/declaredesign/reference/declare_notes.md)
  : Declare quantities computed while a design runs
- [`` `+`( ``*`<dd>`*`)`](https://declaredesign.org/r/declaredesign/reference/plus-.dd.md)
  : Combine design steps into a design

## Helpers

Functions used inside a declaration rather than as a step of it.

- [`label_estimator()`](https://declaredesign.org/r/declaredesign/reference/label_estimator.md)
  [`label_test()`](https://declaredesign.org/r/declaredesign/reference/label_estimator.md)
  : Wrap a custom function as a labeled estimator
- [`tidy_try()`](https://declaredesign.org/r/declaredesign/reference/tidy_try.md)
  : Tidy a model fit, with a fallback
- [`vars()`](https://declaredesign.org/r/declaredesign/reference/vars.md)
  : Capture variables as quosures

## Draw and run

Run a declared design once.

- [`draw_data()`](https://declaredesign.org/r/declaredesign/reference/draw_data.md)
  : Draw the realized data from a design
- [`draw_estimands()`](https://declaredesign.org/r/declaredesign/reference/draw_estimands.md)
  [`draw_estimand()`](https://declaredesign.org/r/declaredesign/reference/draw_estimands.md)
  : Draw the realized estimands
- [`draw_estimates()`](https://declaredesign.org/r/declaredesign/reference/draw_estimates.md)
  : Draw the realized estimates
- [`get_estimates()`](https://declaredesign.org/r/declaredesign/reference/get_estimates.md)
  : Get estimates for a fixed dataset
- [`run_design()`](https://declaredesign.org/r/declaredesign/reference/run_design.md)
  : Execute a design end-to-end

## Diagnose

Run a design many times and summarize how it performs.

- [`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md)
  [`diagnose_designs()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md)
  : Diagnose a design
- [`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)
  [`simulate_designs()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)
  : Simulate one or more designs
- [`diagnose_simulations()`](https://declaredesign.org/r/declaredesign/reference/diagnose_simulations.md)
  : Diagnose a precomputed simulations table
- [`declare_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md)
  : Declare diagnosands
- [`select_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/select_diagnosands.md)
  : Build a diagnosands step from a set of named diagnosands
- [`default_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/default_diagnosands.md)
  : Default diagnosands
- [`get_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/get_diagnosands.md)
  : Get the diagnosands table from a diagnosis
- [`get_simulations()`](https://declaredesign.org/r/declaredesign/reference/get_simulations.md)
  : Get the simulations table from a diagnosis
- [`compare_diagnoses()`](https://declaredesign.org/r/declaredesign/reference/compare_diagnoses.md)
  : Compare the diagnosands of two designs
- [`tidy(`*`<diagnosis>`*`)`](https://declaredesign.org/r/declaredesign/reference/tidy.diagnosis.md)
  : Tidy a diagnosis
- [`pop.var()`](https://declaredesign.org/r/declaredesign/reference/pop.var.md)
  : Population variance

## Redesign

Change the parameters of a declared design.

- [`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
  : Redesign
- [`design_parameters()`](https://declaredesign.org/r/declaredesign/reference/design_parameters.md)
  : Report the parameters a design can be redesigned on
- [`design_notes()`](https://declaredesign.org/r/declaredesign/reference/design_notes.md)
  : Report the notes a design takes

## Inspect

Print and summarize designs and diagnoses.

- [`print(`*`<design>`*`)`](https://declaredesign.org/r/declaredesign/reference/print.design.md)
  : Print a design
- [`summary(`*`<design>`*`)`](https://declaredesign.org/r/declaredesign/reference/summary.design.md)
  : Summarize a design
- [`print(`*`<design_step>`*`)`](https://declaredesign.org/r/declaredesign/reference/print.design_step.md)
  : Print a design step
- [`print(`*`<diagnosis>`*`)`](https://declaredesign.org/r/declaredesign/reference/print.diagnosis.md)
  : Print a diagnosis
- [`summary(`*`<diagnosis>`*`)`](https://declaredesign.org/r/declaredesign/reference/summary.diagnosis.md)
  : Summarize a diagnosis
- [`format(`*`<diagnosis>`*`)`](https://declaredesign.org/r/declaredesign/reference/format.diagnosis.md)
  [`reshape_diagnosis()`](https://declaredesign.org/r/declaredesign/reference/format.diagnosis.md)
  : Format a diagnosis for display
- [`print(`*`<compared_diagnoses>`*`)`](https://declaredesign.org/r/declaredesign/reference/print.compared_diagnoses.md)
  : Print a diagnosand comparison
- [`print(`*`<objects>`*`)`](https://declaredesign.org/r/declaredesign/reference/print.objects.md)
  : Print the objects table, one row per name

## Cite

Attach a citation to a design and retrieve it.

- [`set_citation()`](https://declaredesign.org/r/declaredesign/reference/set_citation.md)
  : Attach a citation to a design
- [`cite_design()`](https://declaredesign.org/r/declaredesign/reference/cite_design.md)
  : Retrieve a design's citation

## Deprecated and defunct

1.x functions kept for porting, each pointing at its replacement.

- [`insert_step()`](https://declaredesign.org/r/declaredesign/reference/insert_step.md)
  : Insert a step into a design (deprecated)

- [`delete_step()`](https://declaredesign.org/r/declaredesign/reference/delete_step.md)
  : Delete a step from a design (deprecated)

- [`replace_step()`](https://declaredesign.org/r/declaredesign/reference/replace_step.md)
  : Replace a step in a design (deprecated)

- [`expand_design()`](https://declaredesign.org/r/declaredesign/reference/expand_design.md)
  : Build a family of designs from a designer function (deprecated)

- [`set_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/set_diagnosands.md)
  : Attach diagnosands to a design (deprecated)

- [`declare_population()`](https://declaredesign.org/r/declaredesign/reference/declare_population.md)
  :

  Deprecated alias for
  [`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md)

- [`model_handler()`](https://declaredesign.org/r/declaredesign/reference/model_handler.md)
  [`tidy_estimator()`](https://declaredesign.org/r/declaredesign/reference/model_handler.md)
  :

  Deprecated alias for
  [`label_estimator()`](https://declaredesign.org/r/declaredesign/reference/label_estimator.md)

- [`declare_potential_outcomes()`](https://declaredesign.org/r/declaredesign/reference/declare-defunct.md)
  [`declare_reveal()`](https://declaredesign.org/r/declaredesign/reference/declare-defunct.md)
  : Defunct step declarations from DeclareDesign 1.x

- [`compare_designs()`](https://declaredesign.org/r/declaredesign/reference/compare_designs.md)
  [`compare_design_code()`](https://declaredesign.org/r/declaredesign/reference/compare_designs.md)
  [`compare_design_data()`](https://declaredesign.org/r/declaredesign/reference/compare_designs.md)
  [`compare_design_estimates()`](https://declaredesign.org/r/declaredesign/reference/compare_designs.md)
  [`compare_design_inquiries()`](https://declaredesign.org/r/declaredesign/reference/compare_designs.md)
  [`compare_design_summaries()`](https://declaredesign.org/r/declaredesign/reference/compare_designs.md)
  [`print_code()`](https://declaredesign.org/r/declaredesign/reference/compare_designs.md)
  : Defunct comparison helpers

## Package

- [`DeclareDesign`](https://declaredesign.org/r/declaredesign/reference/DeclareDesign-package.md)
  [`DeclareDesign-package`](https://declaredesign.org/r/declaredesign/reference/DeclareDesign-package.md)
  : DeclareDesign: A Tidyverse-First Research Design Declaration and
  Diagnosis Tool
