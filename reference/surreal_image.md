# Apply the surreal method to an image file

This function loads an image file, extracts pixel coordinates based on a
brightness threshold, and applies the surreal method to create a dataset
where the image appears in the residual plot.

## Usage

``` r
surreal_image(
  image_path,
  mode = "auto",
  threshold = NULL,
  max_points = NULL,
  invert_y = TRUE,
  R_squared = 0.3,
  p = 5,
  n_add_points = 40,
  max_iter = 100,
  tolerance = 0.01,
  verbose = FALSE
)
```

## Arguments

- image_path:

  Character. Path to an image file or a URL (PNG, JPEG, BMP, TIFF, or
  SVG).

- mode:

  Character. Either `"auto"` (default) to automatically detect, `"dark"`
  to select dark pixels, or `"light"` to select light pixels.

- threshold:

  Numeric or `NULL`. Value between 0 and 1 for grayscale threshold. If
  `NULL` (default), automatically calculated using Otsu's method. For
  `"dark"` mode, pixels below threshold are selected. For `"light"`
  mode, pixels above threshold are selected.

- max_points:

  Integer or `NULL`. Maximum number of points to use. If `NULL`
  (default), automatically estimated based on image size (typically
  2000-5000 points). Set to `Inf` to use all points without
  downsampling.

- invert_y:

  Logical. If `TRUE`, flip y-coordinates so image appears right-side up
  in residual plot. Default is `TRUE`.

- R_squared:

  Numeric. Desired R-squared value. Default is 0.3.

- p:

  Integer. Desired number of columns for matrix X. Default is 5.

- n_add_points:

  Integer. Number of points to add in border transformation. Default is
  40.

- max_iter:

  Integer. Maximum number of iterations for convergence. Default is 100.

- tolerance:

  Numeric. Criteria for detecting convergence and stopping optimization
  early. Default is 0.01.

- verbose:

  Logical. If TRUE, prints progress information. Default is FALSE.

## Value

A `data.frame` containing the results of the surreal method application
with columns `y`, `X1`, `X2`, ..., `Xp`.

## Details

By default, all parameters are automatically detected:

- **mode**: Detected from image histogram (dark subject on light
  background or vice versa)

- **threshold**: Calculated using Otsu's method to optimally separate
  foreground/background

- **max_points**: Estimated based on image dimensions (2000-5000 points)

You can override any of these by specifying explicit values.

**Input Support:**

- Local file paths

- URLs (http:// or https://) - images are downloaded to a temporary file

**Format Support:**

- PNG: Supported via the `png` package (included)

- JPEG: Requires the `jpeg` package

- BMP: Requires the `bmp` package

- TIFF: Requires the `tiff` package

- SVG: Requires the `rsvg` package (renders vector graphics to bitmap)

## See also

[`surreal()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal.md)
for details on the surreal method parameters.
[`surreal_text()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text.md)
for embedding text instead of images.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simplest usage - everything auto-detected
result <- surreal_image("https://www.r-project.org/logo/Rlogo.png")
model <- lm(y ~ ., data = result)
plot(model$fitted, model$residuals, pch = 16)

# Override specific parameters
result <- surreal_image("image.png", mode = "dark", threshold = 0.3)

# Use all points (no downsampling)
result <- surreal_image("image.png", max_points = Inf)
} # }
```
