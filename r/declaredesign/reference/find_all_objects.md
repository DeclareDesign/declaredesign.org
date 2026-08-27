# Find all objects and variables used in a design or design step

This internal function analyzes a DeclareDesign object to find all
variables, functions, and objects that are saved within the design
steps. It identifies objects from quosures, handler environments, and
other design components to provide a comprehensive view of objects the
design depends on.

This function is primarily used internally for design analysis and
debugging purposes. It helps identify dependencies and understand what
objects a design relies on.

## Usage

``` r
find_all_objects(design)
```

## Arguments

- design:

  A design object or design step created using DeclareDesign functions

## Value

A data.frame with columns:

- name:

  The name of the object/variable

- value_str:

  String representation of the object's value or type

- step:

  The step number where the object was found

- quosure:

  The name of the quosure or "handler" where the object was found

- env:

  The environment object where the variable was found
