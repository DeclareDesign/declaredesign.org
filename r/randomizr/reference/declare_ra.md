# Declare a random assignment procedure.

Declare a random assignment procedure.

## Usage

``` r
declare_ra(
  N = NULL,
  blocks = NULL,
  clusters = NULL,
  m = NULL,
  m_unit = NULL,
  m_each = NULL,
  prob = NULL,
  prob_unit = NULL,
  prob_each = NULL,
  block_m = NULL,
  block_m_each = NULL,
  block_prob = NULL,
  block_prob_each = NULL,
  num_arms = NULL,
  conditions = NULL,
  simple = FALSE,
  permutation_matrix = NULL,
  check_inputs = TRUE
)
```

## Arguments

- N:

  The number of units. N must be a positive integer. (required)

- blocks:

  A vector of length N that indicates which block each unit belongs to.

- clusters:

  A vector of length N that indicates which cluster each unit belongs
  to.

- m:

  Use for a two-arm design in which m units (or clusters) are assigned
  to treatment and N-m units (or clusters) are assigned to control. In a
  blocked design, exactly m units in each block will be treated.
  (optional)

- m_unit:

  Use for a two-arm trial. Under complete random assignment, must be
  constant across units. Under blocked random assignment, must be
  constant within blocks.

- m_each:

  Use for a multi-arm design in which the values of m_each determine the
  number of units (or clusters) assigned to each condition. m_each must
  be a numeric vector in which each entry is a nonnegative integer that
  describes how many units (or clusters) should be assigned to the 1st,
  2nd, 3rd... treatment condition. m_each must sum to N. (optional)

- prob:

  Use for a two-arm design in which either floor(N\*prob) or
  ceiling(N\*prob) units (or clusters) are assigned to treatment. The
  probability of assignment to treatment is exactly prob because with
  probability 1-prob, floor(N\*prob) units (or clusters) will be
  assigned to treatment and with probability prob, ceiling(N\*prob)
  units (or clusters) will be assigned to treatment. prob must be a real
  number between 0 and 1 inclusive. (optional)

- prob_unit:

  Use for a two arm design. Must of be of length N. Under simple random
  assignment, can be different for each unit or cluster. Under complete
  random assignment, must be constant across units. Under blocked random
  assignment, must be constant within blocks.

- prob_each:

  Use for a multi-arm design in which the values of prob_each determine
  the probabilities of assignment to each treatment condition. prob_each
  must be a numeric vector giving the probability of assignment to each
  condition. All entries must be nonnegative real numbers between 0 and
  1 inclusive and the total must sum to 1. Because of integer issues,
  the exact number of units assigned to each condition may differ
  (slightly) from assignment to assignment, but the overall probability
  of assignment is exactly prob_each. (optional)

- block_m:

  Use for a two-arm design in which block_m describes the number of
  units to assign to treatment within each block. Note that in previous
  versions of randomizr, block_m behaved like block_m_each.

- block_m_each:

  Use for a multi-arm design in which the values of block_m_each
  determine the number of units (or clusters) assigned to each
  condition. block_m_each must be a matrix with the same number of rows
  as blocks and the same number of columns as treatment arms. Cell
  entries are the number of units (or clusters) to be assigned to each
  treatment arm within each block. The rows should respect the ordering
  of the blocks as determined by sort(unique(blocks)). The columns
  should be in the order of conditions, if specified.

- block_prob:

  Use for a two-arm design in which block_prob describes the probability
  of assignment to treatment within each block. Differs from prob in
  that the probability of assignment can vary across blocks.

- block_prob_each:

  Use for a multi-arm design in which the values of block_prob_each
  determine the probabilities of assignment to each treatment condition.
  block_prob_each must be a matrix with the same number of rows as
  blocks and the same number of columns as treatment arms. Cell entries
  are the probabilities of assignment to treatment within each block.
  The rows should respect the ordering of the blocks as determined by
  sort(unique(blocks)). Use only if the probabilities of assignment
  should vary by block, otherwise use prob_each. Each row of
  block_prob_each must sum to 1.

- num_arms:

  The number of treatment arms. If unspecified, num_arms will be
  determined from the other arguments. (optional)

- conditions:

  A character vector giving the names of the treatment groups. If
  unspecified, the treatment groups will be named 0 (for control) and 1
  (for treatment) in a two-arm trial and T1, T2, T3, in a multi-arm
  trial. An exception is a two-group design in which num_arms is set to
  2, in which case the condition names are T1 and T2, as in a multi-arm
  trial with two arms. (optional)

- simple:

  logical, defaults to FALSE. If TRUE, simple random assignment is used.
  When `simple = TRUE`, please do not specify m, m_each, block_m, or
  block_m_each. If `simple = TRUE`, `prob` and `prob_each` may vary by
  unit.

- permutation_matrix:

  for custom random assignment procedures.

- check_inputs:

  logical. Defaults to TRUE.

## Value

A list of class "declaration". The list has five entries: \$ra_function,
a function that generates random assignments according to the
declaration. \$ra_type, a string indicating the type of random
assignment used \$probabilities_matrix, a matrix with N rows and
num_arms columns, describing each unit's probabilities of assignment to
conditions. \$blocks, the blocking variable. \$clusters, the clustering
variable.

## Examples

``` r
# The declare_ra function is used in three ways:

# 1. To obtain some basic facts about a randomization:
declaration <- declare_ra(N=100, m_each=c(30, 30, 40))
declaration
#> Random assignment procedure: Complete random assignment 
#> Number of units: 100 
#> Number of treatment arms: 3 
#> The possible treatment categories are T1 and T2 and T3.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are constant across units: 
#> prob_T1 prob_T2 prob_T3 
#>     0.3     0.3     0.4 

# 2. To conduct a random assignment:

Z <- conduct_ra(declaration)
table(Z)
#> Z
#> T1 T2 T3 
#> 30 30 40 

# 3. To obtain observed condition probabilities

probs <- obtain_condition_probabilities(declaration, Z)
table(probs, Z)
#>      Z
#> probs T1 T2 T3
#>   0.3 30 30  0
#>   0.4  0  0 40

# Simple Random Assignment Declarations

declare_ra(N=100, simple = TRUE)
#> Random assignment procedure: Simple random assignment 
#> Number of units: 100 
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is 1.26765060022823e+30.  
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 
declare_ra(N=100, prob = .4, simple = TRUE)
#> Random assignment procedure: Simple random assignment 
#> Number of units: 100 
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is 1.26765060022823e+30.  
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.6    0.4 
declare_ra(N=100, prob_each=c(0.3, 0.3, 0.4),
           conditions=c("control", "placebo", "treatment"), simple=TRUE)
#> Random assignment procedure: Simple random assignment 
#> Number of units: 100 
#> Number of treatment arms: 3 
#> The possible treatment categories are control and placebo and treatment.
#> The number of possible random assignments is 5.15377520732011e+47.  
#> The probabilities of assignment are constant across units: 
#>   prob_control   prob_placebo prob_treatment 
#>            0.3            0.3            0.4 

# Complete Random Assignment Declarations

declare_ra(N=100)
#> Random assignment procedure: Complete random assignment 
#> Number of units: 100 
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 
declare_ra(N=100, m_each = c(30, 70),
           conditions = c("control", "treatment"))
#> Random assignment procedure: Complete random assignment 
#> Number of units: 100 
#> Number of treatment arms: 2 
#> The possible treatment categories are control and treatment.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are constant across units: 
#>   prob_control prob_treatment 
#>            0.3            0.7 
declare_ra(N=100, m_each=c(30, 30, 40))
#> Random assignment procedure: Complete random assignment 
#> Number of units: 100 
#> Number of treatment arms: 3 
#> The possible treatment categories are T1 and T2 and T3.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are constant across units: 
#> prob_T1 prob_T2 prob_T3 
#>     0.3     0.3     0.4 


# Block Random Assignment Declarations

blocks <- rep(c("A", "B","C"), times = c(50, 100, 200))

block_m_each <- rbind(c(10, 40),
                 c(30, 70),
                 c(50, 150))
declare_ra(blocks = blocks, block_m_each = block_m_each)
#> Random assignment procedure: Block random assignment 
#> Number of units: 350 
#> Number of blocks: 3
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is approximately infinite. 
#> The probabilities of assignment are NOT constant across units. Your analysis strategy must account for differential probabilities of assignment, typically by employing inverse probability weights.


# Cluster Random Assignment Declarations

clusters <- rep(letters, times = 1:26)
declare_ra(clusters = clusters)
#> Random assignment procedure: Cluster random assignment 
#> Number of units: 351 
#> Number of clusters: 26
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is 10400600.  
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 
declare_ra(clusters = clusters, m_each = c(7, 7, 12))
#> Random assignment procedure: Cluster random assignment 
#> Number of units: 351 
#> Number of clusters: 26
#> Number of treatment arms: 3 
#> The possible treatment categories are T1 and T2 and T3.
#> The number of possible random assignments is 33145226400.  
#> The probabilities of assignment are constant across units: 
#>   prob_T1   prob_T2   prob_T3 
#> 0.2692308 0.2692308 0.4615385 

# Blocked and Clustered Random Assignment Declarations

clusters <- rep(letters, times=1:26)
blocks <- rep(NA, length(clusters))
blocks[clusters %in% letters[1:5]] <- "block_1"
blocks[clusters %in% letters[6:10]] <- "block_2"
blocks[clusters %in% letters[11:15]] <- "block_3"
blocks[clusters %in% letters[16:20]] <- "block_4"
blocks[clusters %in% letters[21:26]] <- "block_5"

table(blocks, clusters)
#>          clusters
#> blocks     a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w
#>   block_1  1  2  3  4  5  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   block_2  0  0  0  0  0  6  7  8  9 10  0  0  0  0  0  0  0  0  0  0  0  0  0
#>   block_3  0  0  0  0  0  0  0  0  0  0 11 12 13 14 15  0  0  0  0  0  0  0  0
#>   block_4  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 16 17 18 19 20  0  0  0
#>   block_5  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 21 22 23
#>          clusters
#> blocks     x  y  z
#>   block_1  0  0  0
#>   block_2  0  0  0
#>   block_3  0  0  0
#>   block_4  0  0  0
#>   block_5 24 25 26

declare_ra(clusters = clusters, blocks = blocks)
#> Random assignment procedure: Blocked and clustered random assignment 
#> Number of units: 351 
#> Number of blocks: 5
#> Number of clusters: 26
#> Number of treatment arms: 2 
#> The possible treatment categories are 0 and 1.
#> The number of possible random assignments is 3200000.  
#> The probabilities of assignment are constant across units: 
#> prob_0 prob_1 
#>    0.5    0.5 
declare_ra(clusters = clusters, blocks = blocks, prob_each = c(.2, .5, .3))
#> Random assignment procedure: Blocked and clustered random assignment 
#> Number of units: 351 
#> Number of blocks: 5
#> Number of clusters: 26
#> Number of treatment arms: 3 
#> The possible treatment categories are T1 and T2 and T3.
#> The number of possible random assignments is 6.144e+09.  
#> The probabilities of assignment are constant across units: 
#> prob_T1 prob_T2 prob_T3 
#>     0.2     0.5     0.3 
```
