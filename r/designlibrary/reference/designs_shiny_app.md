# Launch the DesignLibrary Shiny browser

Launch the DesignLibrary Shiny browser

## Usage

``` r
designs_shiny_app(...)
```

## Arguments

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

The value of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) (called
for side effects).

## Examples

``` r
if (interactive()) {
  designs_shiny_app()
}
```
