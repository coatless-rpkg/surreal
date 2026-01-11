# Changelog

## surreal 0.0.2

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
