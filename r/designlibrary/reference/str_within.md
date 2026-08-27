# Takes substring between matched strings. Avoids dependency on stringr package.

Takes substring between matched strings. Avoids dependency on stringr
package.

## Usage

``` r
str_within(string, pattern = "^(structure\\()|(, \\.Names)")
```

## Arguments

- string:

  A string. String from which substring is extracted.

- pattern:

  A regular expression that matches the beggining and end of a substring

## Value

Substring within `string` surrounded by matched `pattern`.
