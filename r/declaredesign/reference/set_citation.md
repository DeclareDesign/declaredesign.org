# Set the citation of a design

Set the citation of a design

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

  A design typically created using the + operator

- title:

  The title of the design, as a character string.

- author:

  The author(s) of the design, as a character string.

- year:

  The year of the design, as a character string.

- description:

  A description of the design in words, as a character string.

- citation:

  (optional) The preferred citation for the design, as a character
  string, in which case title, author, year, and description may be left
  unspecified.

## Value

a design object with a citation attribute

## Examples

``` r

# Setup for example
design <-
  declare_model(data = sleep) +
  declare_sampling(S = complete_rs(N, n = 10))

# Set citation using set_citation
design <-
  set_citation(design,
               author = "Lovelace, Ada",
               title = "Notes",
               year = 1953,
               description = 
                 "This is a text description of a design")

# View citation information using cite_design
cite_design(design)
#> @Unpublished{,
#>   title = {Notes},
#>   author = {{Lovelace} and {Ada}},
#>   note = {This is a text description of a design},
#>   year = {1953},
#> }
```
