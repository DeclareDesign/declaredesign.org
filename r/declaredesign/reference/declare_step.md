# Declare a custom data-handling step

Wraps an arbitrary handler function as a step in the design. The handler
must accept `data` as its first argument and return a data frame.

## Usage

``` r
declare_step(handler, ..., label = "custom_step", draws = 1L)
```

## Arguments

- handler:

  (required) A function whose first argument is `data`.

- ...:

  (optional) Additional arguments passed to `handler` **as written**, so
  the handler evaluates them itself. A bare column name arrives as a
  name, which is what lets `tidyr::pivot_wider(id_cols = pair)` and the
  dplyr verbs select and mask as they normally do. This is the same rule
  [`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md)
  follows. A handler that does no evaluation of its own and wants a
  value computed from the data takes a closure instead:
  `declare_step(handler = function(data) f(data, cutoff = mean(data$a)))`.

- label:

  (optional) Step label. Defaults to `"custom_step"`.

- draws:

  (optional) Number of nested draws for this step. Defaults to `1`. When
  `> 1`, the step is re-executed `draws` times for each upstream draw
  during nested simulation.

## Value

A `design_step` (class `c("design_step", "dd", "function")`): a function
that takes the current data frame and returns what `handler` returns for
it. Add it to a design with `+`.

## See also

Other design declarations:
[`declare_assignment()`](https://declaredesign.org/r/declaredesign/reference/declare_assignment.md),
[`declare_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.md),
[`declare_inquiry()`](https://declaredesign.org/r/declaredesign/reference/declare_inquiry.md),
[`declare_measurement()`](https://declaredesign.org/r/declaredesign/reference/declare_measurement.md),
[`declare_model()`](https://declaredesign.org/r/declaredesign/reference/declare_model.md),
[`declare_notes()`](https://declaredesign.org/r/declaredesign/reference/declare_notes.md),
[`declare_parameters()`](https://declaredesign.org/r/declaredesign/reference/declare_parameters.md),
[`declare_sampling()`](https://declaredesign.org/r/declaredesign/reference/declare_sampling.md),
[`declare_test()`](https://declaredesign.org/r/declaredesign/reference/declare_test.md)

## Examples

``` r
step <- declare_step(handler = function(data, k) {
  data$X2 <- data$X * k
  data
}, k = 2)
df <- data.frame(X = 1:5)
step(df)
#>   X X2
#> 1 1  2
#> 2 2  4
#> 3 3  6
#> 4 4  8
#> 5 5 10

# A tidyselect handler receives the column names, not their contents.
wide <- declare_step(
  id_cols = pair, names_from = role, values_from = c(ID, a),
  handler = tidyr::pivot_wider
)
long <- data.frame(pair = rep(1:3, each = 2), role = rep(c("A", "B"), 3),
                   ID = sprintf("%03d", 1:6), a = 1:6)
wide(long)
#> # A tibble: 3 × 5
#>    pair ID_A  ID_B    a_A   a_B
#>   <int> <chr> <chr> <int> <int>
#> 1     1 001   002       1     2
#> 2     2 003   004       3     4
#> 3     3 005   006       5     6
```
