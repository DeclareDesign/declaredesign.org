# Package index

## Declare

These functions declare research design steps

- [`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md)
  : Declare the size and features of the population.
- [`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md)
  [`declare_inquiries()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md)
  [`declare_estimand()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md)
  [`declare_estimands()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md)
  [`inquiry_handler()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md)
  : Declare inquiry
- [`declare_sampling()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md)
  [`sampling_handler()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md)
  : Declare sampling procedure
- [`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md)
  [`assignment_handler()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md)
  : Declare Data Strategy: Assignment
- [`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md)
  [`measurement_handler()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md)
  [`potential_outcomes_handler()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md)
  : Declare measurement procedure
- [`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
  [`declare_estimators()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
  [`label_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
  [`method_handler()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
  : Declare estimator
- [`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)
  [`label_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)
  : Declare test
- [`declare_step()`](https://declaredesign.org/r/declaredesign/reference/declare_step.md)
  : Declare a custom step
- [`set_citation()`](https://declaredesign.org/r/declaredesign/reference/set_citation.md)
  : Set the citation of a design
- [`tidy_try()`](https://declaredesign.org/r/declaredesign/reference/tidy_try.md)
  : Tidy Model Results and Filter to Relevant Coefficients
- [`` `+`( ``*`<dd>`*`)`](https://declaredesign.org/r/declaredesign/reference/declare_design.md)
  : Declare a design

## Post-declaration functions

These functions operate on declared designs

- [`cite_design()`](https://declaredesign.org/r/declaredesign/reference/cite_design.md)
  : Obtain the preferred citation for a design
- [`draw_data()`](https://declaredesign.org/r/declaredesign/reference/draw_functions.md)
  [`draw_estimand()`](https://declaredesign.org/r/declaredesign/reference/draw_functions.md)
  [`draw_estimands()`](https://declaredesign.org/r/declaredesign/reference/draw_functions.md)
  [`draw_estimates()`](https://declaredesign.org/r/declaredesign/reference/draw_functions.md)
  : Draw data, estimates, and inquiries from a design
- [`run_design()`](https://declaredesign.org/r/declaredesign/reference/run_design.md)
  : Run a design one time
- [`get_estimates()`](https://declaredesign.org/r/declaredesign/reference/get_functions.md)
  : Get estimates, inquiries, assignment vectors, or samples from a
  design given data
- [`print_code()`](https://declaredesign.org/r/declaredesign/reference/post_design.md)
  [`print(`*`<design>`*`)`](https://declaredesign.org/r/declaredesign/reference/post_design.md)
  [`summary(`*`<design>`*`)`](https://declaredesign.org/r/declaredesign/reference/post_design.md)
  : Explore your design

## Diagnose

These functions assist with research design diagnosis

- [`diagnosand_handler()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md)
  [`declare_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/declare_diagnosands.md)
  : Declare diagnosands
- [`diagnose_design()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md)
  [`diagnose_designs()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md)
  [`vars()`](https://declaredesign.org/r/declaredesign/reference/diagnose_design.md)
  : Diagnose the design
- [`get_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/diagnosis_helpers.md)
  [`get_simulations()`](https://declaredesign.org/r/declaredesign/reference/diagnosis_helpers.md)
  : Explore your design diagnosis
- [`simulate_design()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)
  [`simulate_designs()`](https://declaredesign.org/r/declaredesign/reference/simulate_design.md)
  : Simulate a design
- [`set_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/set_diagnosands.md)
  : Set the diagnosands for a design
- [`select_diagnosands()`](https://declaredesign.org/r/declaredesign/reference/select_diagnosands.md)
  : Select diagnosands
- [`reshape_diagnosis()`](https://declaredesign.org/r/declaredesign/reference/reshape_diagnosis.md)
  : Clean up a diagnosis object for printing
- [`tidy(`*`<diagnosis>`*`)`](https://declaredesign.org/r/declaredesign/reference/tidy.diagnosis.md)
  : Tidy diagnosis
- [`pop.var()`](https://declaredesign.org/r/declaredesign/reference/pop.var.md)
  : Population variance function

## Redesign

These functions modify declared designs

- [`insert_step()`](https://declaredesign.org/r/declaredesign/reference/modify_design.md)
  [`delete_step()`](https://declaredesign.org/r/declaredesign/reference/modify_design.md)
  [`replace_step()`](https://declaredesign.org/r/declaredesign/reference/modify_design.md)
  : Modify a design after the fact
- [`expand_design()`](https://declaredesign.org/r/declaredesign/reference/expand_design.md)
  : Declare a design via a designer
- [`redesign()`](https://declaredesign.org/r/declaredesign/reference/redesign.md)
  : Redesign

## Comparison

These functions compare declared designs

- [`compare_diagnoses()`](https://declaredesign.org/r/declaredesign/reference/compare_diagnoses.md)
  : Compare Diagnoses
- [`compare_designs()`](https://declaredesign.org/r/declaredesign/reference/compare_functions.md)
  [`compare_design_code()`](https://declaredesign.org/r/declaredesign/reference/compare_functions.md)
  [`compare_design_summaries()`](https://declaredesign.org/r/declaredesign/reference/compare_functions.md)
  [`compare_design_data()`](https://declaredesign.org/r/declaredesign/reference/compare_functions.md)
  [`compare_design_estimates()`](https://declaredesign.org/r/declaredesign/reference/compare_functions.md)
  [`compare_design_inquiries()`](https://declaredesign.org/r/declaredesign/reference/compare_functions.md)
  : Compare two designs

## Package

Package description

- [`DeclareDesign-package`](https://declaredesign.org/r/declaredesign/reference/DeclareDesign.md)
  [`DeclareDesign`](https://declaredesign.org/r/declaredesign/reference/DeclareDesign.md)
  : DeclareDesign package
