test_that("process_image() returns the black pixels as x and y from the bottom left", {
  image <- array(1, dim = c(3, 4, 3))
  image[1, 2, ] <- 0
  image[3, 4, ] <- 0

  expect_equal(process_image(image), list(x = c(2, 4), y = c(3, 1)))
})

test_that("process_image() skips pixels that are dark without being black", {
  image <- array(1, dim = c(3, 4, 3))
  image[2, 2, ] <- 0.1
  image[3, 1, ] <- c(0, 0, 1)

  expect_length(process_image(image)$x, 0)
})

test_that("temporary_text_plot() draws the text in black on a bitmap", {
  skip_if_not(capabilities("png"))

  image <- temporary_text_plot("Hi")
  black <- process_image(image)

  expect_length(dim(image), 3)
  expect_gte(dim(image)[3], 3)
  expect_gte(min(image), 0)
  expect_lte(max(image), 1)
  expect_gt(length(black$x), 0)
})

test_that("temporary_text_plot() draws more pixels for larger text", {
  skip_if_not(capabilities("png"))

  small <- process_image(temporary_text_plot("Hi", cex = 2))
  large <- process_image(temporary_text_plot("Hi", cex = 6))

  expect_gt(length(large$x), length(small$x))
})

test_that("surreal_text_points() returns the pixels of the text as x and y", {
  skip_if_not(capabilities("png"))
  pixels <- process_image(temporary_text_plot("Hi"))

  points <- surreal_text_points("Hi")

  expect_s3_class(points, "data.frame")
  expect_named(points, c("x", "y"))
  expect_equal(points$x, pixels$x)
  expect_equal(points$y, pixels$y)
})

test_that("surreal_text() hides the points that surreal_text_points() returns", {
  skip_if_not(capabilities("png"))
  points <- surreal_text_points("Hi", cex = 3)

  direct <- withr::with_seed(3, surreal_text("Hi", cex = 3))
  by_hand <- withr::with_seed(3, surreal(points))

  expect_equal(direct, by_hand)
})

test_that("surreal_text() returns a row for each pixel of the text and its frame", {
  skip_if_not(capabilities("png"))
  withr::local_seed(3)
  pixels <- process_image(temporary_text_plot("Hi"))

  hidden <- surreal_text("Hi", p = 2, n_add_points = 5)

  expect_named(hidden, c("y", "X.1", "X.2"))
  expect_equal(nrow(hidden), length(pixels$x) + 4 * 5)
})

test_that("surreal_text() leaves the text as the residuals of the full model", {
  skip_if_not(capabilities("png"))
  withr::local_seed(3)
  pixels <- process_image(temporary_text_plot("Hi"))
  framed <- border_augmentation(pixels$x, pixels$y, n_add_points = 5)

  hidden <- surreal_text("Hi", n_add_points = 5)
  fit <- lm(y ~ ., data = hidden)

  expect_equal(unname(residuals(fit)), framed[, 2] - mean(framed[, 2]))
})

test_that("surreal_text() says so when the text draws nothing", {
  skip_if_not(capabilities("png"))

  expect_snapshot(error = TRUE, surreal_text(""))
})

test_that("surreal_text_points() says so when the text draws nothing", {
  skip_if_not(capabilities("png"))

  expect_snapshot(error = TRUE, surreal_text_points(" "))
})
