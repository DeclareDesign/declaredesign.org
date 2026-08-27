# Compare two designs

Compare two designs

## Usage

``` r
compare_designs(
  design1,
  design2,
  format = "ansi8",
  pager = "off",
  context = -1L,
  rmd = FALSE
)

compare_design_code(
  design1,
  design2,
  format = "ansi256",
  mode = "sidebyside",
  pager = "off",
  context = -1L,
  rmd = FALSE
)

compare_design_summaries(
  design1,
  design2,
  format = "ansi256",
  mode = "sidebyside",
  pager = "off",
  context = -1L,
  rmd = FALSE
)

compare_design_data(
  design1,
  design2,
  format = "ansi256",
  mode = "sidebyside",
  pager = "off",
  context = -1L,
  rmd = FALSE
)

compare_design_estimates(
  design1,
  design2,
  format = "ansi256",
  mode = "auto",
  pager = "off",
  context = -1L,
  rmd = FALSE
)

compare_design_inquiries(
  design1,
  design2,
  format = "ansi256",
  mode = "sidebyside",
  pager = "off",
  context = -1L,
  rmd = FALSE
)
```

## Arguments

- design1:

  A design object, typically created using the + operator

- design2:

  A design object, typically created using the + operator

- format:

  Format (in console or HTML) options from
  [`diffobj::diffChr`](https://rdrr.io/pkg/diffobj/man/diffChr.html)

- pager:

  Pager option from
  [`diffobj::diffChr`](https://rdrr.io/pkg/diffobj/man/diffChr.html)

- context:

  Context option from
  [`diffobj::diffChr`](https://rdrr.io/pkg/diffobj/man/diffChr.html)
  which sets the number of lines around differences that are printed. By
  default, all lines of the two objects are shown. To show only the
  lines that are different, set `context = 0`; to get one line around
  differences for context, set to 1.

- rmd:

  Set to `TRUE` use in Rmarkdown HTML output. NB: will not work with
  LaTeX, Word, or other .Rmd outputs.

- mode:

  Mode options from
  [`diffobj::diffChr`](https://rdrr.io/pkg/diffobj/man/diffChr.html)

## Examples

``` r

design1 <- declare_model(N = 100, u = rnorm(N), potential_outcomes(Y ~ Z + u)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N, n = 75)) +
  declare_assignment(Z = complete_ra(N, m = 50)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")

  design2 <- declare_model(N = 200, U = rnorm(N),
                         potential_outcomes(Y ~ 0.5*Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = complete_rs(N, n = 100)) +
  declare_assignment(Z = complete_ra(N, m = 25)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, .method = lm_robust, inquiry = "ATE")
 
 if (require("diffobj")) {
 
   compare_designs(design1, design2)
   compare_design_code(design1, design2)
   compare_design_summaries(design1, design2)
   compare_design_data(design1, design2)
   compare_design_estimates(design1, design2)
   compare_design_inquiries(design1, design2)
 
 }
#> Loading required package: diffobj
#> < design1             > design2           
#> @@ 1,2 @@             @@ 1,2 @@           
#>     inquiry estimand      inquiry estimand
#> < 1     ATE        1  > 1     ATE      0.5
```
