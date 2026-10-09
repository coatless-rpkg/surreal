#' Create a Temporary Text Plot
#'
#' This function creates a temporary png image file containing a plot of the
#' given text.
#'
#' @param text Character. A plain text message to be plotted.
#' @param cex  Numeric. A value specifying the relative size of the text. Default is 4.
#'
#' @return
#' An array containing data from the temporary image file.
#'
#' @importFrom grDevices png dev.off
#' @importFrom graphics plot text
#'
#' @noRd
#'
#' @examples
#' temp_file <- temporary_text_plot("Hello, World!")
temporary_text_plot <- function(text, cex = 4) {

  # Replace empty spaces with double dots for better visibility
  # text <- gsub("", "..", text)

  # Create a temporary file path using a known directory
  temp_dir <- tempdir()
  temp_file <- tempfile(tmpdir = temp_dir, fileext = ".png")

  # Ensure the temporary file is removed when the function exits
  on.exit(unlink(temp_file))

  # Create a bitmap image
  png(temp_file, antialias = "none")

  # Create a blank plot
  plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")

  # Add text to the plot
  text(1, 1, text, cex = cex)

  # Close the plotting device
  dev.off()

  # Read the image file
  image <- png::readPNG(temp_file)

  image
}

#' Process Image for Text Embedding
#'
#' This function processes a temporary image file created by `temporary_text_plot()`.
#' It extracts the pixel data and converts it into x and y coordinates.
#'
#' @param image An array containing the image plot.
#'
#' @return
#' A list with two elements:
#'
#' \describe{
#'   \item{x}{Numeric vector of x coordinates}
#'   \item{y}{Numeric vector of y coordinates}
#' }
#'
#' @noRd
#'
#' @examples
#' temp_file <- temporary_text_plot("Hello, World!")
#' image_data <- process_image(temp_file)
process_image <- function(image) {

  # Convert the array to integer values between 0 and 255
  img_array <- round(image * 255)

  # Find black points (where all channels are 0)
  activated_points <- which(
    img_array[,,1] == 0 &
    img_array[,,2] == 0 &
    img_array[,,3] == 0,
    arr.ind = TRUE)

  # Return coordinates of text pixels
  list(
    x = activated_points[, 2],  # Column index represents x
    y = nrow(img_array) - activated_points[, 1] + 1  # Row index represents y, but flipped
  )
}

#' Find the points that draw a text string
#'
#' Does the work of [`surreal_text_points()`]. An error is reported as coming
#' from the function the user called, which is `call`.
#'
#' @inheritParams surreal_text_points
#' @param call The environment of the function to name in an error.
#'
#' @return A data.frame of `x` and `y` coordinates.
#'
#' @noRd
text_points <- function(text, cex, call = parent.frame()) {

  # Create temporary plot of the text
  image <- temporary_text_plot(text = text, cex = cex)

  # Process the image to extract coordinate data
  image_data <- process_image(image)

  if (length(image_data$x) == 0) {
    cli::cli_abort(
      c(
        "{.arg text} draws no points.",
        "i" = "Use text with at least one visible character."
      ),
      call = call
    )
  }

  data.frame(x = image_data$x, y = image_data$y)
}

#' Turn text into the points that draw it
#'
#' This function draws the text on a temporary bitmap and returns the position
#' of every pixel the text covers. These are the points that [`surreal_text()`]
#' hides. Getting them first lets you look at them, change them, or combine
#' them with other points before handing them to [`surreal()`].
#'
#' @param text Character. A plain text message to be plotted. Default is "hello world".
#' @param cex  Numeric. A value specifying the relative size of the text. Default is 4.
#'
#' @return
#' A data.frame with one row for each point of the text and two columns,
#' `x` and `y`.
#'
#' @examples
#' # The points that draw "R is fun"
#' points <- surreal_text_points("R is fun")
#' plot(points, pch = 16, asp = 1)
#'
#' # Hide them in a dataset, as surreal_text() does
#' result <- surreal(points)
#'
#' @seealso
#' [`surreal_text()`] to go from text to a dataset in one step.
#' [`surreal_image_points()`] for the points of an image.
#'
#' @export
surreal_text_points <- function(text = "hello world", cex = 4) {
  text_points(text = text, cex = cex)
}

#' Apply the surreal method to a text string
#'
#' This function applies the surreal method to a text string. It first finds
#' the points that draw the text with [`surreal_text_points()`], and then
#' applies the surreal method to them.
#'
#' @inheritParams surreal_text_points
#' @inheritParams surreal
#'
#' @return
#' A data.frame containing the results of the surreal method application.
#'
#' @examples
#' # Create a surreal plot of the text "R is fun" appearing on one line
#' r_is_fun_result <- surreal_text("R is fun", verbose = TRUE)
#'
#' # Create a surreal plot of the text "Statistics Rocks" by using an escape
#' # character to create a second line between "Statistics" and "Rocks"
#' stat_rocks_result <- surreal_text("Statistics\nRocks", verbose = TRUE)
#'
#' @seealso
#' [`surreal()`] for details on the surreal method parameters.
#' [`surreal_text_points()`] for the points of the text on their own.
#'
#' @export
surreal_text <- function(text = "hello world",
                         cex = 4,
                         R_squared = 0.3, p = 5,
                         n_add_points = 40,
                         max_iter = 100, tolerance = 0.01, verbose = FALSE) {

  # Find the points that draw the text
  points <- text_points(text = text, cex = cex)

  # Apply the surreal method to the points
  result <- surreal(
    R_0 = points$y, y_hat = points$x,
    R_squared = R_squared, p = p, n_add_points = n_add_points,
    max_iter = max_iter, tolerance = tolerance, verbose = verbose
  )

  return(result)
}
