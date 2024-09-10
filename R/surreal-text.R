#' Create a Temporary Text Plot
#'
#' This function creates a temporary png image file containing a plot of the
#' given text.
#'
#' @param text A character string to be plotted.
#' @param cex A numeric value specifying the relative size of the text. Default is 4.
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
#' @param image A character vector containing the bitmap image plot.
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

#' Apply the surreal method to a text string
#'
#' This function applies the surreal method to a text string. It first creates a
#' temporary plot with the text, processes the image, and then applies the surreal
#' method to the data.
#'
#' @param text A character string to apply the surreal method to
#' @param cex A numeric value for the size of the text
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
#'
#' @export
surreal_text <- function(text = "hello world",
                         cex = 4,
                         R_squared = 0.3, p = 5,
                         n_add_points = 40,
                         max_iter = 100, tolerance = 0.01, verbose = FALSE) {

  # Create temporary plot of the text
  temp_file <- temporary_text_plot(text = text, cex = cex)

  # Process the image to extract coordinate data
  image_data <- process_image(temp_file)

  # Apply the surreal method to the extracted data
  result <- surreal(
    R_0 = image_data$y, y_hat = image_data$x,
    R_squared = R_squared, p = p, n_add_points = n_add_points,
    max_iter = max_iter, tolerance = tolerance, verbose = verbose
  )

  return(result)
}
