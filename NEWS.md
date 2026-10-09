# surreal (development version)

## New Features

- `surreal_decoys()` adds predictors of pure noise to a dataset, so the hidden
  image shows only for the model with the real predictors. `surreal_path()`
  then runs forward selection and records every step: which predictor
  entered, the BIC or AIC, the coefficients and the residuals. Its `plot()`
  method draws the coefficient paths, the criterion and the residual plot at
  a step.

- `surreal_trace()` records how the method finds its data: the fitted values at
  every iteration of the search, how far they were from their targets and how
  much each iteration changed. Its `plot()` method draws the picture as it
  stood at an iteration. A `step` below 1 slows the search down to watch it.

- `surreal_text_points()` and `surreal_image_points()` return the points that
  draw a message or an image, as `x` and `y` coordinates. These are the points
  `surreal_text()` and `surreal_image()` hide, so you can look at them, change
  them or combine them before handing them to `surreal()`.

- The Shiny app has a demo that runs in a web browser, with nothing to install:
  <https://r-pkg.thecoatlessprofessor.com/surreal/demo/>. The documentation
  site builds it with Shinylive.

## Improvements

- `surreal()`, `surreal_text()` and `surreal_image()` are much faster and use
  far less memory on large pictures. A picture of 5,000 points took about 20
  seconds and over a gigabyte of memory, and now takes a fraction of a second.
  The data they return for a given seed is unchanged.

## Bug Fixes

- `surreal_app()`: the Code dialog shows code that runs when the message has
  a quotation mark or a line break in it.

- `surreal_app()`: the download buttons hand over their files when the app runs
  in a web browser through Shinylive.

- `surreal_app()`: the Source pane, the Source download and Undo show the
  message that was generated. The pane kept the first message when a second
  one was generated.

- `surreal_image()`: a picture in two light tones, such as light gray on
  white, is found. The automatic threshold fell just under the darker tone,
  so no points were selected.

- `surreal_image()`: an image address with a query string, such as
  `picture.jpg?raw=1`, is read in its own format. It was saved and read as a
  PNG.

- `surreal_text()`: text with no visible characters gives an error that says
  so.

# surreal 0.0.2

## New Features

- `surreal_image()`: Create surreal datasets directly from image files or URLs.
  Supports PNG, JPEG, BMP, TIFF, and SVG formats with automatic mode detection
  and threshold calculation.

- `surreal_app()`: Launch an interactive Shiny application for exploring the
  surreal algorithm. Includes demo datasets, custom text input, image uploads,
  and real-time parameter controls. Export results to CSV or download plots.

# surreal 0.0.1

## Features

- `surreal()`: embeds a hidden image supplied by (x, y) coordinates
  into a data set that seemingly has no pattern until the residuals are plotted.
- `surreal_text()`: embeds a hidden text pattern into a data set that seemingly
  has no pattern until the residuals are plotted.
- Included data:
  - `r_logo_image_data`: a data set containing the R logo image that can be
    hidden using the `surreal()` function.
  - `jack_o_lantern_image_data`: a data set containing a hidden jack-o-lantern 
    image.
