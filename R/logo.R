#' Load a 2D data set with the shape of the R logo
#'
#' This function is a helper function to load the R logo data included under
#' the `inst/` directory.
#'
#' @return
#' A data frame with the x and y coordinates of the R logo
#'
#' @export
#' @examples
#' # Load the R logo data
#' data <- r_logo_example()
#'
#' # Plot the R logo
#' plot(data$x, data$y, pch = 16, main = "R Logo", xlab = '', ylab = '')
#' @references
#' Staudenmayer, J. (2007). Hidden Images in R. Retrieved from
#' <https://www4.stat.ncsu.edu/~stefansk/NSF_Supported/Hidden_Images/000_R_Programs/John_Staudenmayer/logo.txt>
r_logo_example <- function() {
  # Load the R logo text file data
  data <- scan(system.file("samples/r-logo.txt", package = "surreal"), quiet = TRUE)

  # Convert to scatterplot format
  # by reading the first two lines of the data file for dimensions and
  # then reading the rest of the file for the data points
  y <- rep(1:data[2], each = data[1])
  x <- rep(1:data[1], times = data[2])
  y <- -y[data[-(1:2)] == 1]
  x <- x[data[-(1:2)] == 1]

  # Place inside of a data frame
  data <- data.frame(x = x, y = y)

  return(data)
}
