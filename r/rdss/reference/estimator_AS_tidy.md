# Tidy helper function for estimator_AS function

Runs estimates estimation function from interference package and returns
tidy data frame output

## Usage

``` r
estimator_AS_tidy(data, permutatation_matrix, adj_matrix)
```

## Arguments

- data:

  a data.frame

- permutatation_matrix:

  a permutation matrix of random assignments

- adj_matrix:

  an adjacency matrix

## Value

a data.frame of estimates

## Details

The estimator_AS_tidy function requires the 'interference' package,
which is not yet available on CRAN.

To use this function:

1.  install the developer version of interference via
    remotes::install_github('szonszein/interference') and

2.  install the developer version of rdss via
    remotes::install_github('DeclareDesign/rdss@remotes')

See
https://book.declaredesign.org/experimental-causal.html#experiments-over-networks
