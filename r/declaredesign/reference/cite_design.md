# Retrieve a design's citation

Returns the citation
[`set_citation()`](https://declaredesign.org/r/declaredesign/reference/set_citation.md)
attached to the design, or, when none was attached, the citation for the
DeclareDesign package itself.

## Usage

``` r
cite_design(design)
```

## Arguments

- design:

  (required) A `design`.

## Value

The design's citation, a `bibentry` or a string, or
`utils::citation("DeclareDesign")` when none has been set.

## See also

Other citing a design:
[`set_citation()`](https://declaredesign.org/r/declaredesign/reference/set_citation.md)

## Examples

``` r
design <- set_citation(declare_model(N = 10, Y = rnorm(N)),
                       title = "Example", author = "Coppock", year = 2026)
cite_design(design)
#> Coppock (2026). “Example.” Unpublished research design declaration.
```
