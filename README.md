
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surreal <img src="man/figures/logo-surreal.png" align ="right" alt="A hexagonal logo of the surreal R package that shows a series of points with varying sizes" width ="150"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/coatless-rpkg/surreal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coatless-rpkg/surreal/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `surreal` package seeks to implement the methods inside the paper
“Residual Sur(Realism)” by [Leonard A.
Stefanski](https://www4.stat.ncsu.edu/~stefansk/)
(<doi:10.1198/000313007X190079>). These methods allow for hiding images
inside of residual plots.

## Installation

You can install the development version of surreal from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("coatless-rpkg/surreal")
```

## Example

To begin, first load the package:

``` r
library(surreal)
```

Next, we’ll retrieve R logo image included with the package:

``` r
# Load the R logo data included with the package
data("r_logo_image_data", package = "surreal")

# Display original data
plot(r_logo_image_data, pch = 16, main = "Original data")
```

<img src="man/figures/README-load-logo-1.png" width="100%" />

We can see that the data is in a 2D format.

``` r
str(r_logo_image_data)
#> 'data.frame':    2000 obs. of  2 variables:
#>  $ x: int  54 55 56 57 58 59 34 35 36 49 ...
#>  $ y: int  -9 -9 -9 -9 -9 -9 -10 -10 -10 -10 ...
summary(r_logo_image_data)
#>        x                y         
#>  Min.   :  5.00   Min.   :-75.00  
#>  1st Qu.: 32.00   1st Qu.:-57.00  
#>  Median : 57.00   Median :-39.00  
#>  Mean   : 55.29   Mean   :-40.48  
#>  3rd Qu.: 77.00   3rd Qu.:-24.00  
#>  Max.   :100.00   Max.   : -9.00
```

We can apply the surreal method to the data:

``` r
# Apply the surreal method to the data
transformed_surreal_data <- surreal(r_logo_image_data)
```

After applying the surreal method, we get a series of additional
predictors that when visualized against `y` look as if they are no
underlying patterns.

``` r
# View the expanded data after transformation
pairs(y ~ ., data = transformed_surreal_data, main = "Data after transformation")
```

<img src="man/figures/README-surreal-method-data-pair-plot-1.png" width="100%" />

Finally, we’ll fit a linear model to the transformed data and plot the
residuals:

``` r
# Fit a linear model to the transformed data
model <- lm(y ~ ., data = transformed_surreal_data)

# Plot the residuals
plot(model$fitted, model$resid, type = "n", main = "Residual plot from transformed data")
points(model$fitted, model$resid, pch = 16)
```

<img src="man/figures/README-surreal-method-residual-plot-1.png" width="100%" />

From the residual plot, we get back our original R logo that has a
slight border around it to improve the original image recovery.

## Acknowledgements

Prior work was done to bring the algorithms into R by [John
Staudenmayer](https://www4.stat.ncsu.edu/~stefansk/NSF_Supported/Hidden_Images/000_R_Programs/John_Staudenmayer/),
[Peter
Wolf](https://www4.stat.ncsu.edu/~stefansk/NSF_Supported/Hidden_Images/000_R_Programs/Peter_Wolf/),
and [Ulrike
Gromping](https://www4.stat.ncsu.edu/~stefansk/NSF_Supported/Hidden_Images/000_R_Programs/Ulrike_Gromping/).
