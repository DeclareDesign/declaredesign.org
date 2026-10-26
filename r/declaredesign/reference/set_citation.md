# Attach a citation to a design

Records how the design should be cited, as a
[`utils::bibentry()`](https://rdrr.io/r/utils/bibentry.html) built from
the fields given, or as the text in `citation`.
[`cite_design()`](https://declaredesign.org/r/declaredesign/reference/cite_design.md)
returns it.

## Usage

``` r
set_citation(
  design,
  title = NULL,
  author = NULL,
  year = NULL,
  description = "Unpublished research design declaration",
  citation = NULL
)
```

## Arguments

- design:

  (required) A `design`.

- title:

  (optional) Title of the design.

- author:

  (optional) Author or authors, as a string or a
  [`utils::person()`](https://rdrr.io/r/utils/person.html).

- year:

  (optional) Year.

- description:

  (optional) Note on the entry. Defaults to
  `"Unpublished research design declaration"`.

- citation:

  (optional) A ready-made citation, as a string or a `bibentry`, used in
  place of the fields above.

## Value

`design`, with the citation stored in its `citation` attribute.

## See also

Other citing a design:
[`cite_design()`](https://declaredesign.org/r/declaredesign/reference/cite_design.md)

## Examples

``` r
design <- declare_model(N = 10, Y = rnorm(N))
design <- set_citation(design, title = "Example", author = "Coppock",
                       year = 2026)
cite_design(design)
#> Coppock (2026). “Example.” Unpublished research design declaration.
```
