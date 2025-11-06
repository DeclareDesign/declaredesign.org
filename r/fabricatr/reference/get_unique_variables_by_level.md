# Find which variables are unique at a given level in hierarchical data

Find which variables are unique at a given level in hierarchical data

## Usage

``` r
get_unique_variables_by_level(data, ID_label, superset = NULL)
```

## Arguments

- data:

  a data.frame

- ID_label:

  the ID label to split upon

- superset:

  Superset contains a vector of character strings that contain variables
  the modify level call is going to write. Some of these may be columns
  in the data frame, others might not be. If superset is specified, then
  we definitely only want to check those variables

## Value

a character vector enumerating the unique variables
