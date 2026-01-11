# Verify suggested packages are available

Checks if the required packages are available. If not, an error message
is thrown.

## Usage

``` r
require_packages(packages)
```

## Arguments

- packages:

  Character vector of package names

## Value

Stops with an error message if any of the required packages are missing.
Otherwise, returns `TRUE` invisibly.
