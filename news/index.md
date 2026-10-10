# Changelog

## surreal (development version)

## surreal 0.0.3

CRAN release: 2026-10-09

### New features

- [`surreal_app()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_app.md)
  has a demo that runs in a web browser through Shinylive, with nothing
  to install, at <https://r-pkg.thecoatlessprofessor.com/surreal/demo/>
  ([\#5](https://github.com/coatless-rpkg/surreal/issues/5)).

- [`surreal_app()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_app.md)
  gains two tabs. Search plays back the search that makes a dataset, and
  Selection steps through forward selection among decoy predictors
  ([\#6](https://github.com/coatless-rpkg/surreal/issues/6),
  [\#7](https://github.com/coatless-rpkg/surreal/issues/7)).

- [`surreal_decoys()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_decoys.md)
  adds predictors of pure noise to a dataset, so the hidden image shows
  only for the model with the real predictors
  ([\#6](https://github.com/coatless-rpkg/surreal/issues/6)).

- [`surreal_path()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_path.md)
  runs forward selection and records every step. Its
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method draws
  the share each step explained against the criterion’s charge for a
  predictor, the criterion and the residual plot at a step
  ([\#6](https://github.com/coatless-rpkg/surreal/issues/6),
  [\#7](https://github.com/coatless-rpkg/surreal/issues/7)).

- [`surreal_text_points()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text_points.md)
  and
  [`surreal_image_points()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image_points.md)
  return the points that
  [`surreal_text()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text.md)
  and
  [`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md)
  hide, as `x` and `y` coordinates
  ([\#6](https://github.com/coatless-rpkg/surreal/issues/6)).

- [`surreal_trace()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_trace.md)
  records every iteration of the search that makes a dataset, and its
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method draws
  the image as it stood at an iteration. A `step` below 1 slows the
  search down
  ([\#6](https://github.com/coatless-rpkg/surreal/issues/6)).

### Minor improvements and bug fixes

- [`surreal()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal.md),
  [`surreal_text()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text.md)
  and
  [`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md)
  are much faster and use far less memory on large images. The data they
  return for a given seed is unchanged
  ([\#6](https://github.com/coatless-rpkg/surreal/issues/6)).

- [`surreal_app()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_app.md)
  download buttons hand over their files when the app runs in a web
  browser ([\#5](https://github.com/coatless-rpkg/surreal/issues/5)).

- [`surreal_app()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_app.md)
  shows the generated message in the Source pane, in its download and
  after Undo ([\#5](https://github.com/coatless-rpkg/surreal/issues/5)).

- [`surreal_app()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_app.md)
  shows code that runs when the message has a quotation mark or a line
  break in it
  ([\#6](https://github.com/coatless-rpkg/surreal/issues/6)).

- [`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md)
  finds an image drawn in two light tones, such as light gray on white
  ([\#6](https://github.com/coatless-rpkg/surreal/issues/6)).

- [`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md)
  reads an image address with a query string, such as
  `picture.jpg?raw=1`, in its own format
  ([\#6](https://github.com/coatless-rpkg/surreal/issues/6)).

- [`surreal_text()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text.md)
  gives a clear error for text with no visible characters
  ([\#6](https://github.com/coatless-rpkg/surreal/issues/6)).

## surreal 0.0.2

CRAN release: 2026-01-11

### New Features

- [`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md):
  Create surreal datasets directly from image files or URLs. Supports
  PNG, JPEG, BMP, TIFF, and SVG formats with automatic mode detection
  and threshold calculation.

- [`surreal_app()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_app.md):
  Launch an interactive Shiny application for exploring the surreal
  algorithm. Includes demo datasets, custom text input, image uploads,
  and real-time parameter controls. Export results to CSV or download
  plots.

## surreal 0.0.1

CRAN release: 2024-09-12

### Features

- [`surreal()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal.md):
  embeds a hidden image supplied by (x, y) coordinates into a data set
  that seemingly has no pattern until the residuals are plotted.
- [`surreal_text()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text.md):
  embeds a hidden text pattern into a data set that seemingly has no
  pattern until the residuals are plotted.
- Included data:
  - `r_logo_image_data`: a data set containing the R logo image that can
    be hidden using the
    [`surreal()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal.md)
    function.
  - `jack_o_lantern_image_data`: a data set containing a hidden
    jack-o-lantern image.
