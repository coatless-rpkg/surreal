# surreal 0.0.1

## Features

- `surreal()`: embeds a hidden image supplied by (x, y) coordinates
  into a data set that seemingly has no pattern until the residuals are plotted.
- `surreal_text()`: embeds a hidden text pattern into a data set that seemingly
  has no pattern until the residuals are plotted.
  - This function works only on Unix-like systems due to the `ppm` file type
    not being supported by the version of GhostScript included with R on Windows.
- Included data:
  - `r_logo_image_data`: a data set containing the R logo image that can be
    hidden using the `surreal()` function.
  - `jack_o_lantern_image_data`: a data set containing a hidden jack-o-lantern 
    image.
