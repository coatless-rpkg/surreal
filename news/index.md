# Changelog

## surreal (development version)

### New Features

- [`surreal_decoys()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_decoys.md)
  adds predictors of pure noise to a dataset, so the hidden image shows
  only for the model with the real predictors.
  [`surreal_path()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_path.md)
  then runs forward selection and records every step: which predictor
  entered, the BIC or AIC, the coefficients and the residuals. Its
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method draws
  the coefficient paths, the criterion and the residual plot at a step.

- [`surreal_trace()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_trace.md)
  records the search that makes the data: the fitted values at every
  iteration, their distance from their targets and the size of each
  change. Its [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  method draws the picture as it stood at an iteration. A `step` below 1
  slows the search down to watch it.

- [`surreal_app()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_app.md)
  has two new tabs. Search plays back the search that makes the data,
  one iteration at a time, with a step size to slow it down. Selection
  adds decoy predictors and steps through forward selection, with the
  coefficient paths, the criterion and the residual plot at each step.

- [`surreal_text_points()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text_points.md)
  and
  [`surreal_image_points()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image_points.md)
  return the points that draw a message or an image, as `x` and `y`
  coordinates. These are the points
  [`surreal_text()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text.md)
  and
  [`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md)
  hide, so you can look at them, change them or combine them before
  handing them to
  [`surreal()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal.md).

- The Shiny app has a demo that runs in a web browser, with nothing to
  install: <https://r-pkg.thecoatlessprofessor.com/surreal/demo/>. The
  documentation site builds it with Shinylive.

### Improvements

- [`surreal()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal.md),
  [`surreal_text()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text.md)
  and
  [`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md)
  are much faster and use far less memory on large pictures. A picture
  of 5,000 points took about 20 seconds and over a gigabyte of memory,
  and now takes a fraction of a second. The data they return for a given
  seed is unchanged.

### Bug Fixes

- [`surreal_app()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_app.md):
  the Code dialog shows code that runs when the message has a quotation
  mark or a line break in it.

- [`surreal_app()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_app.md):
  the download buttons hand over their files when the app runs in a web
  browser through Shinylive.

- [`surreal_app()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_app.md):
  the Source pane, the Source download and Undo show the message that
  was generated. The pane kept the first message when a second one was
  generated.

- [`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md):
  a picture in two light tones, such as light gray on white, is found.
  The automatic threshold fell just under the darker tone, so no points
  were selected.

- [`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md):
  an image address with a query string, such as `picture.jpg?raw=1`, is
  read in its own format. It was saved and read as a PNG.

- [`surreal_text()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text.md):
  text with no visible characters gives an error that says so.

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
