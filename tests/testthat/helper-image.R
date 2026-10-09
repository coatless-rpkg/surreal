# A white picture, 40 pixels tall and 60 wide, with a black square of 20 pixels a side.
square_image <- function() {
  image <- matrix(1, nrow = 40, ncol = 60)
  image[11:30, 21:40] <- 0
  image
}

# The picture written to a PNG file that is removed when the test ends.
local_square_png <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".png", .local_envir = env)
  png::writePNG(square_image(), path)
  path
}
