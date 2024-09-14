#' R Logo Pixel Data
#'
#' 2D data set with the shape of the R Logo in x and y coordinate pairings.
#'
#' @format
#' A data frame with 2,000 observations and 2 variables describing the
#' x and y coordinates of the R logo.
#'
#' @examples
#' # Load the R logo data
#' data("r_logo_image_data", package = "surreal")
#'
#' # Plot the R logo
#' plot(r_logo_image_data$x, r_logo_image_data$y, pch = 16, main = "R Logo", xlab = '', ylab = '')
#' @references
#' Staudenmayer, J. (2007). Hidden Images in R. Retrieved from
#' <https://www4.stat.ncsu.edu/~stefansk/NSF_Supported/Hidden_Images/000_R_Programs/John_Staudenmayer/logo.txt>
"r_logo_image_data"

#' Jack-o'-Lantern Surreal Data
#'
#' Data set containing a hidden image of a Jack-o'-Lantern lurking
#' in the residual plot of a full model being fit.
#'
#' @format
#' A data frame with 5,395 observations and 7 variables.
#'
#' - `y`: Response variable
#' - `x1`: Predictor variable 1
#' - `x2`: Predictor variable 2
#' - `x3`: Predictor variable 3
#' - `x4`: Predictor variable 4
#' - `x5`: Predictor variable 5
#' - `x6`: Predictor variable 6
#'
#' @examples
#' # Load the Jack-o'-Lantern data
#' data <- jackolantern_surreal_data
#'
#' # Fit a linear model to the surreal Jack-o'-Lantern data
#' model <- lm(y ~ ., data = data)
#'
#' # Plot the residuals to reveal the hidden image
#' plot(model$fitted, model$resid, type = "n", main = "Residual plot from transformed data")
#' points(model$fitted, model$resid, pch = 16)
#' @references
#' Stefansk, L.A. (2013). Hidden Images in the Helen Barton Lecture Series. Retrieved from
#' <https://www4.stat.ncsu.edu/~stefansk/NSF_Supported/Hidden_Images/UNCG_Helen_Barton_Lecture_Nov_2013/pumpkin_1_data_yx1x6.txt>
"jackolantern_surreal_data"

