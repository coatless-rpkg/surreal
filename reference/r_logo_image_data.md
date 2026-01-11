# R Logo Pixel Data

2D data set with the shape of the R Logo in x and y coordinate pairings.

## Usage

``` r
r_logo_image_data
```

## Format

A data frame with 2,000 observations and 2 variables describing the x
and y coordinates of the R logo.

## References

Staudenmayer, J. (2007). Hidden Images in R. Retrieved from
<https://www4.stat.ncsu.edu/~stefansk/NSF_Supported/Hidden_Images/000_R_Programs/John_Staudenmayer/logo.txt>

## Examples

``` r
# Load the R logo data
data("r_logo_image_data", package = "surreal")

# Plot the R logo
plot(r_logo_image_data$x, r_logo_image_data$y, pch = 16, main = "R Logo", xlab = '', ylab = '')
```
