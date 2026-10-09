# Turn an image into the points that draw it

This function loads an image file and returns the position of every
pixel that passes a brightness threshold. These are the points that
[`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md)
hides. Getting them first lets you look at them, change them, or combine
them with other points before handing them to
[`surreal()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal.md).

## Usage

``` r
surreal_image_points(
  image_path,
  mode = "auto",
  threshold = NULL,
  max_points = NULL,
  invert_y = TRUE,
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

- verbose:

  Logical. If TRUE, prints progress information. Default is FALSE.

## Value

A data.frame with one row for each point of the image and two columns,
`x` and `y`.

## Details

The mode, the threshold and the number of points are chosen
automatically unless you set them, as described for
[`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md).

## See also

[`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md)
to go from an image to a dataset in one step.
[`surreal_text_points()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text_points.md)
for the points of a text message.

## Examples

``` r
if (FALSE) { # \dontrun{
# The points of the R logo
points <- surreal_image_points("https://www.r-project.org/logo/Rlogo.png")
plot(points, pch = 16, asp = 1)

# Hide them in a dataset, as surreal_image() does
result <- surreal(points)
} # }
```
