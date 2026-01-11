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
