#' Create a Temporary Text Plot
#'
#' This function creates a temporary bitmap image file containing a plot of the given text.
#'
#' @param text A character string to be plotted.
#' @param cex A numeric value specifying the relative size of the text. Default is 4.
#'
#' @return
#' A character string containing the path to the temporary image file.
#'
#' @importFrom grDevices bitmap dev.off
#' @importFrom graphics plot text
#' @keywords internal
#' @export
#'
#' @examples
#' temp_file <- temporary_text_plot("Hello, World!")
temporary_text_plot <- function(text, cex = 4) {

  # Replace empty spaces with double dots for better visibility
  # text <- gsub("", "..", text)

  # Create a temporary file path
  temp_file <- tempfile()

  # Create a bitmap image
  bitmap(temp_file, "ppm", height = 5, width = 5)

  # Create a blank plot
  plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")

  # Add text to the plot
  text(1, 1, text, cex = cex)

  # Close the plotting device
  dev.off()

  # Return the path to the temporary file
  temp_file
}

#' Process Image for Text Embedding
#'
#' This function processes a temporary image file created by `temporary_text_plot()`.
#' It extracts the pixel data and converts it into x and y coordinates.
#'
#' @param file_path A character string specifying the path to the temporary
#'                  bitmap image plot.
#'
#' @return
#' A list with two elements:
#'
#' \describe{
#'   \item{x}{Numeric vector of x coordinates}
#'   \item{y}{Numeric vector of y coordinates}
#' }
#' @keywords internal
#' @export
#'
#' @examples
#' temp_file <- temporary_text_plot("Hello, World!")
#' image_data <- process_image(temp_file)
process_image <- function(file_path) {
  # Read the image file
  image <- scan(file_path, "", sep = "\n", quiet = TRUE)

  # Extract image size from the third line
  size <- as.numeric(unlist(strsplit(image[3], " ")))

  # Extract pixel data (skip first 4 lines of metadata)
  pixel_data <- image[-(1:4)]
  pixel_data <- unlist(lapply(strsplit(pixel_data, " "), as.numeric))

  # Convert pixel data to a matrix
  pixel_matrix <- matrix(pixel_data, ncol = 3, byrow = TRUE)

  # Convert to binary (text pixels are where any RGB value is 0)
  pixel_matrix <- pixel_matrix[,1] == 0 | pixel_matrix[,2] == 0 | pixel_matrix[,3] == 0
  pixel_matrix <- matrix(pixel_matrix, nrow = size[1], ncol = size[2], byrow = TRUE)

  # Remove empty rows and columns
  pixel_matrix <- pixel_matrix[apply(pixel_matrix, 1, any), ]
  pixel_matrix <- pixel_matrix[, apply(pixel_matrix, 2, any)]

  # Generate x and y coordinates
  x <- col(pixel_matrix)
  y <- nrow(pixel_matrix) + 1 - row(pixel_matrix)

  # Return coordinates of text pixels
  list(x = x[pixel_matrix], y = y[pixel_matrix])
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
