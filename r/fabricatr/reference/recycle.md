# Recycle a vector to the length of the level being built

fabricatr 1.x's helper for filling a level with a short vector. 2.0
recycles automatically whenever the vector's length divides the level
evenly, so this is only ever a no-op made explicit, and is kept so
designs written for 1.x run unchanged.

## Usage

``` r
recycle(x, .N = NULL)
```

## Arguments

- x:

  A vector.

- .N:

  Length to recycle to. Left off inside a level.

## Value

\`x\`, recycled to length \`.N\`.

## Details

Left off, \`.N\` is the \`N\` of the level being built. 1.x found it by
walking the call stack; a 2.0 column expression is evaluated against a
data mask instead, so \`N\` is read out of the calling frame, which is
that mask.

## Examples

``` r
fabricate(villages = add_level(N = 6, arm = recycle(c("a", "b", "c"))))
#> # A tibble: 6 × 2
#>   villages arm  
#>   <chr>    <chr>
#> 1 1        a    
#> 2 2        b    
#> 3 3        c    
#> 4 4        a    
#> 5 5        b    
#> 6 6        c    
recycle(c("a", "b"), .N = 6)
#> [1] "a" "b" "a" "b" "a" "b"
```
