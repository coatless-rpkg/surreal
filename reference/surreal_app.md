# Launch the Surreal Shiny App

Opens an interactive Shiny application for exploring the surreal
algorithm. The app allows you to generate datasets with hidden images in
residual plots using demo data, custom text, or uploaded images.

## Usage

``` r
surreal_app(launch.browser = TRUE, port = NULL, host = "127.0.0.1")
```

## Arguments

- launch.browser:

  Logical. If `TRUE` (default), opens the app in the default web
  browser. If `FALSE`, returns the app URL for manual opening.

- port:

  Integer. The port to run the app on. If `NULL` (default), Shiny will
  choose an available port.

- host:

  Character. The host address. Default is `"127.0.0.1"` (localhost).

## Value

This function is called for its side effect of launching the Shiny app.
It does not return a value.

## Details

The app provides:

- Demo datasets (Jack-o-Lantern, R Logo)

- Custom text input to embed messages in residual plots

- Image upload support (PNG, JPEG, BMP, TIFF, SVG)

- Interactive controls for R², predictors, and image processing settings

- Dark/light mode toggle

- Data export to CSV

- A Search tab that plays back the search that makes the data, one
  iteration at a time

- A Selection tab that hides the image among decoy predictors and
  selects the real ones one step at a time

## Requirements

The app requires the shiny and bslib packages to be installed. For image
uploads, additional packages may be needed depending on the format:

- JPEG: jpeg

- BMP: bmp

- TIFF: tiff

- SVG: rsvg

## See also

[`surreal()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal.md)
for the core algorithm.
[`surreal_text()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text.md)
for embedding text programmatically.
[`surreal_image()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image.md)
for processing images programmatically.
[`surreal_trace()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_trace.md)
and
[`surreal_path()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_path.md)
for the functions behind the Search and Selection tabs.

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the app in the default browser
surreal_app()

# Launch on a specific port
surreal_app(port = 3838)

# Get the app without launching browser
surreal_app(launch.browser = FALSE)
} # }
```
