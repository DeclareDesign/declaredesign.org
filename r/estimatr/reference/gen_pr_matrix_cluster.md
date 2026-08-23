# Generate condition probability matrix given clusters and probabilities

Generate condition probability matrix given clusters and probabilities

## Usage

``` r
gen_pr_matrix_cluster(clusters, treat_probs, simple)
```

## Arguments

- clusters:

  A vector of clusters

- treat_probs:

  A vector of treatment (condition 2) probabilities

- simple:

  A boolean for whether the assignment is a random sample assignment
  (TRUE, default) or complete random assignment (FALSE)

## Value

a numeric 2n\*2n matrix of marginal and joint condition treatment
probabilities to be passed to the `condition_pr_mat` argument of
[`horvitz_thompson`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md).

## See also

[`declaration_to_condition_pr_mat`](https://declaredesign.org/r/estimatr/reference/declaration_to_condition_pr_mat.md)
