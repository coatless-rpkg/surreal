# Plot a Recorded Selection

Draws three panels for one step of a selection recorded by
[`surreal_path()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_path.md):
the coefficient of every predictor along the path, the criterion along
the path, and the residual plot of the model at the step.

## Usage

``` r
# S3 method for class 'surreal_path'
plot(x, step = x$best, ...)
```

## Arguments

- x:

  A `surreal_path` object.

- step:

  Integer. The step to draw, from 0 to the number of predictors. Default
  is the best step.

- ...:

  Further arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for the
  residual plot.

## Value

`x`, invisibly. Called for the plot it draws.

## Details

A solid line marks the step that is drawn, and a dashed line the best
step. Decoy predictors, when the path knows which they are, are drawn in
gray.

## Examples

``` r
set.seed(114)
hidden <- surreal(r_logo_image_data)
path <- surreal_path(surreal_decoys(hidden, n = 20))

plot(path)

plot(path, step = 25)

```
