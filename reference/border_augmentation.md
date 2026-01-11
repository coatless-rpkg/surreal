# Transform Data by Adding a Border

This function transforms the input data by adding points around the
original data to create a frame. It uses an optimization process to find
the best alpha parameter for point distribution, which helps in making
the fitted values and residuals orthogonal.

## Usage

``` r
border_augmentation(x, y, n_add_points = 40, verbose = FALSE)
```

## Arguments

- x:

  Numeric vector of x coordinates.

- y:

  Numeric vector of y coordinates.

- n_add_points:

  Integer. Number of points to add on each side of the frame. Default is
  `40`.

- verbose:

  Logical. If `TRUE`, prints optimization progress. Default is `FALSE`.

## Value

A matrix with two columns representing the transformed `x` and `y`
coordinates.

## Examples

``` r
# Simulate data
x <- rnorm(100)
y <- rnorm(100)

# Append border to data
transformed_data <- border_augmentation(x, y)

# Modify par settings for plotting side-by-side
oldpar <- par(mfrow = c(1, 2))

# Graph original and transformed data
plot(x, y, pch = 16, main = "Original data")
plot(
  transformed_data[, 1], transformed_data[, 2], pch = 16,
  main = "Transformed data", xlab = 'x', ylab = 'y'
)


# Restore original par settings
par(oldpar)
```
