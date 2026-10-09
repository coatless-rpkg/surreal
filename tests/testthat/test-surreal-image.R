test_that("is_url() recognizes web addresses and nothing else", {
  expect_equal(
    is_url(c(
      "https://example.com/a.png",
      "HTTP://example.com",
      "a.png",
      "ftp://example.com"
    )),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("download_to_temp() saves the file under its own extension", {
  path <- local_square_png()

  saved <- download_to_temp(paste0("file://", path))
  withr::defer(unlink(saved))

  expect_equal(tools::file_ext(saved), "png")
  expect_equal(png::readPNG(saved), square_image())
})

test_that("download_to_temp() says which address it could not fetch", {
  missing <- paste0("file://", withr::local_tempfile(fileext = ".png"))

  expect_error(
    suppressWarnings(download_to_temp(missing)),
    "Failed to download image from URL"
  )
})

test_that("load_image_file() reads a PNG as an array of values from 0 to 1", {
  path <- local_square_png()

  image <- load_image_file(path)

  expect_equal(dim(image), c(40, 60))
  expect_equal(range(image), c(0, 1))
})

test_that("load_image_file() reads a JPEG when the jpeg package is installed", {
  skip_if_not_installed("jpeg")
  path <- withr::local_tempfile(fileext = ".jpg")
  jpeg::writeJPEG(square_image(), path, quality = 1)

  image <- load_image_file(path)

  expect_equal(dim(image)[1:2], c(40, 60))
})

test_that("load_image_file() explains a missing file and an unknown format", {
  unknown <- withr::local_tempfile(fileext = ".gif")
  file.create(unknown)

  expect_snapshot(error = TRUE, {
    load_image_file("no-such-image.png")
    load_image_file(unknown)
  })
})

test_that("image_to_grayscale() returns a grayscale image unchanged", {
  expect_identical(image_to_grayscale(square_image()), square_image())
})

test_that("image_to_grayscale() weights red, green and blue by luminance", {
  image <- array(0, dim = c(2, 2, 3))
  image[,, 1] <- 1

  red <- image_to_grayscale(image)

  expect_equal(red, matrix(0.299, 2, 2))
})

test_that("image_to_grayscale() ignores an alpha channel", {
  image <- array(0.5, dim = c(2, 2, 4))
  image[,, 4] <- 0

  expect_equal(image_to_grayscale(image), matrix(0.5, 2, 2))
})

test_that("image_to_grayscale() takes the first channel of gray with alpha", {
  image <- array(c(rep(0.25, 4), rep(1, 4)), dim = c(2, 2, 2))

  expect_equal(image_to_grayscale(image), matrix(0.25, 2, 2))
})

test_that("image_to_grayscale() rejects an array that is not an image", {
  expect_snapshot(
    error = TRUE,
    image_to_grayscale(array(0, dim = c(2, 2, 3, 2)))
  )
})

test_that("auto_detect_mode() looks for the subject against the background", {
  mostly_light <- square_image()

  expect_equal(auto_detect_mode(mostly_light), "dark")
  expect_equal(auto_detect_mode(1 - mostly_light), "light")
})

test_that("otsu_threshold() falls between the two groups of gray levels", {
  image <- matrix(rep(c(0.2, 0.8), each = 50), nrow = 10)

  threshold <- otsu_threshold(image)

  expect_gt(threshold, 0.2)
  expect_lt(threshold, 0.8)
})

test_that("auto_max_points() keeps every point of a small picture", {
  expect_null(auto_max_points(n_extracted = 500, img_area = 100 * 100))
})

test_that("auto_max_points() caps a large picture between 2000 and 5000 points", {
  expect_equal(auto_max_points(n_extracted = 9000, img_area = 100 * 100), 2000L)
  expect_equal(auto_max_points(n_extracted = 9000, img_area = 600 * 600), 3000L)
  expect_equal(
    auto_max_points(n_extracted = 9000, img_area = 2000 * 2000),
    5000L
  )
})

test_that("extract_points_from_image() takes the dark pixels, with y counted from the bottom", {
  image <- matrix(1, nrow = 3, ncol = 4)
  image[1, 2] <- 0
  image[3, 4] <- 0

  points <- extract_points_from_image(image, "dark", 0.5, invert_y = TRUE)

  expect_equal(points, list(x = c(2, 4), y = c(3, 1)), ignore_attr = TRUE)
})

test_that("extract_points_from_image() counts y from the top when it is not inverted", {
  image <- matrix(1, nrow = 3, ncol = 4)
  image[1, 2] <- 0
  image[3, 4] <- 0

  points <- extract_points_from_image(image, "dark", 0.5, invert_y = FALSE)

  expect_equal(points, list(x = c(2, 4), y = c(1, 3)), ignore_attr = TRUE)
})

test_that("extract_points_from_image() takes the light pixels in light mode", {
  image <- matrix(0, nrow = 3, ncol = 4)
  image[2, 3] <- 1

  points <- extract_points_from_image(image, "light", 0.5, invert_y = TRUE)

  expect_equal(points, list(x = 3, y = 2), ignore_attr = TRUE)
})

test_that("extract_points_from_image() says so when no pixel passes the threshold", {
  expect_snapshot(
    error = TRUE,
    extract_points_from_image(matrix(1, 3, 4), "dark", 0.5, invert_y = TRUE)
  )
})

test_that("downsample_points() keeps every point at or under the limit", {
  points <- list(x = 1:10, y = 10:1)

  expect_identical(downsample_points(points, max_points = NULL), points)
  expect_identical(downsample_points(points, max_points = 10), points)
})

test_that("downsample_points() draws the limit's worth of matching x and y", {
  withr::local_seed(5)
  points <- list(x = 1:100, y = 101:200)

  fewer <- downsample_points(points, max_points = 25)

  expect_length(fewer$x, 25)
  expect_equal(fewer$y - fewer$x, rep(100, 25))
  expect_equal(anyDuplicated(fewer$x), 0)
})

test_that("downsample_points() reports what it did when verbose", {
  withr::local_seed(5)
  points <- list(x = 1:100, y = 101:200)

  expect_snapshot({
    kept <- downsample_points(points, max_points = NULL, verbose = TRUE)
    fewer <- downsample_points(points, max_points = 25, verbose = TRUE)
  })
})

test_that("surreal_image() returns a row for each dark pixel and its frame", {
  path <- local_square_png()
  withr::local_seed(11)

  hidden <- surreal_image(path, max_points = Inf, p = 2, n_add_points = 5)

  expect_named(hidden, c("y", "X.1", "X.2"))
  expect_equal(nrow(hidden), 20 * 20 + 4 * 5)
})

test_that("surreal_image() leaves the picture as the residuals of the full model", {
  path <- local_square_png()
  withr::local_seed(11)
  pixels <- extract_points_from_image(
    square_image(),
    "dark",
    0.5,
    invert_y = TRUE
  )
  framed <- border_augmentation(pixels$x, pixels$y, n_add_points = 5)

  hidden <- surreal_image(path, max_points = Inf, n_add_points = 5)
  fit <- lm(y ~ ., data = hidden)

  expect_equal(unname(residuals(fit)), framed[, 2] - mean(framed[, 2]))
})

test_that("surreal_image() keeps every point of a small picture by default", {
  path <- local_square_png()
  withr::local_seed(11)

  hidden <- surreal_image(path, n_add_points = 5)

  expect_equal(nrow(hidden), 20 * 20 + 4 * 5)
})

test_that("surreal_image() keeps no more than max_points of the picture", {
  path <- local_square_png()
  withr::local_seed(11)

  hidden <- surreal_image(path, max_points = 100, n_add_points = 5)

  expect_equal(nrow(hidden), 100 + 4 * 5)
})

test_that("surreal_image() takes the light pixels when asked to", {
  path <- local_square_png()
  withr::local_seed(11)

  hidden <- surreal_image(
    path,
    mode = "light",
    max_points = Inf,
    n_add_points = 5
  )

  expect_equal(nrow(hidden), 40 * 60 - 20 * 20 + 4 * 5)
})

test_that("surreal_image() reports each step when verbose", {
  path <- local_square_png()
  withr::local_seed(11)
  withr::local_pdf(NULL)

  reported <- capture_messages(capture_output(
    surreal_image(path, max_points = 100, n_add_points = 5, verbose = TRUE)
  ))

  expect_match(reported, "Auto-detected mode", all = FALSE)
  expect_match(reported, "Auto-detected threshold", all = FALSE)
  expect_match(reported, "Downsampling from", all = FALSE)
  expect_match(reported, "Applying surreal method", all = FALSE)
})

test_that("surreal_image() rejects arguments it cannot use", {
  path <- local_square_png()

  expect_snapshot(error = TRUE, {
    surreal_image(c("a.png", "b.png"))
    surreal_image(path, threshold = 2)
    surreal_image(path, max_points = 0)
  })
})

test_that("surreal_image() takes only the modes it knows", {
  path <- local_square_png()

  expect_error(surreal_image(path, mode = "dim"), "should be one of")
})
