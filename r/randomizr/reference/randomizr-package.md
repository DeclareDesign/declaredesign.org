# randomizr: Easy-to-Use Tools for Common Forms of Random Assignment and Sampling

randomizr generates random assignments for common experimental designs
and random samples for common sampling designs. The functions are named
for the procedure they implement, and each has a \`\_probabilities\`
companion that returns the probability of each unit falling into each
condition, which is what inverse-probability weights are built from.

## Random assignment

- \[simple_ra()\] assigns each unit independently, so the number treated
  varies from draw to draw.

- \[complete_ra()\] fixes the number treated on every draw.

- \[block_ra()\] conducts complete assignment separately within blocks
  of similar units, which increases precision.

- \[cluster_ra()\] assigns whole groups together, for interventions that
  cannot be delivered to individuals.

- \[block_and_cluster_ra()\] does both at once.

- \[balanced_ra()\] (experimental) holds condition counts (and, with
  `formula`, covariate totals) at their targets while keeping each
  unit's probability exact.

- \[declare_ra()\] describes a design once so it can be reused by
  \[conduct_ra()\] to draw assignments and by
  \[obtain_condition_probabilities()\] to recover the probabilities.
  Balanced assignment is opt-in: `ra_type = "balanced"`,
  `prob_unit_each`, or `formula`.

## Random sampling

The sampling functions mirror the assignment ones: \[simple_rs()\],
\[complete_rs()\], \[strata_rs()\], \[cluster_rs()\] and
\[strata_and_cluster_rs()\], with \[declare_rs()\], \[draw_rs()\] and
\[obtain_inclusion_probabilities()\] playing the roles that
\[declare_ra()\], \[conduct_ra()\] and
\[obtain_condition_probabilities()\] play for assignment.

## Randomization inference

\[obtain_permutation_matrix()\] enumerates or samples the assignments a
design could have produced, and \[obtain_num_permutations()\] counts
them.

## References

Blair, G., Cooper, J., Coppock, A. and Humphreys, M. (2019). Declaring
and Diagnosing Research Designs. *American Political Science Review*
113(3), 838-859.
[doi:10.1017/S0003055419000194](https://doi.org/10.1017/S0003055419000194)

Gerber, A. S. and Green, D. P. (2012). *Field Experiments: Design,
Analysis, and Interpretation*. New York: W. W. Norton.

## See also

Useful links:

- <https://declaredesign.org/r/randomizr/>

- <https://github.com/DeclareDesign/randomizr>

- Report bugs at <https://github.com/DeclareDesign/randomizr/issues>

## Author

**Maintainer**: Alexander Coppock <acoppock@gmail.com>
([ORCID](https://orcid.org/0000-0002-5733-2386))

Authors:

- Alexander Coppock <acoppock@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-5733-2386))

Other contributors:

- Jasper Cooper <jaspercooper@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-8639-3188)) \[contributor\]

- Neal Fultz <nfultz@gmail.com> (C version of restricted partitions)
  \[contributor\]

- Graeme Blair <graeme.blair@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-9164-2102)) \[contributor\]

- Macartan Humphreys <macartan@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-7029-2326)) \[contributor\]

## Examples

``` r
# Complete random assignment: exactly 50 of 100 units treated, every draw.
Z <- complete_ra(N = 100, m = 50)
table(Z)
#> Z
#>  0  1 
#> 50 50 

# Blocking on a covariate usually buys precision.
blocks <- rep(c("small", "large"), times = c(60, 40))
Z <- block_ra(blocks = blocks)
table(blocks, Z)
#>        Z
#> blocks   0  1
#>   large 20 20
#>   small 30 30

# Declare once, then draw and recover probabilities from the same object.
declaration <- declare_ra(N = 100, m = 50)
Z <- conduct_ra(declaration)
probs <- obtain_condition_probabilities(declaration, Z)
table(probs)
#> probs
#> 0.5 
#> 100 
```
