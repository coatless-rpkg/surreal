# Plot a Recorded Search

Draws the picture as it stood at one iteration of a search recorded by
[`surreal_trace()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_trace.md),
or the path the search took.

## Usage

``` r
# S3 method for class 'surreal_trace'
plot(x, iteration = nrow(x$iterations), type = c("picture", "trace"), ...)
```

## Arguments

- x:

  A `surreal_trace` object.

- iteration:

  Integer. The iteration to draw, or to mark on the path. Default is the
  last one.

- type:

  Character. `"picture"` (default) plots the fitted values of the
  iteration against the residuals. `"trace"` plots the distance of the
  fitted values from their targets at every iteration, on a log scale.

- ...:

  Further arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

`x`, invisibly. Called for the plot it draws.

## Examples

``` r
set.seed(114)
trace <- surreal_trace(r_logo_image_data)

plot(trace, iteration = 2)

plot(trace, type = "trace")

```
