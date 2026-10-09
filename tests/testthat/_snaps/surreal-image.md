# load_image_file() explains a missing file and an unknown format

    Code
      load_image_file("no-such-image.png")
    Condition
      Error in `load_image_file()`:
      ! Image file not found: 'no-such-image.png'
      i Check that the file path is correct.
    Code
      load_image_file(unknown)
    Condition
      Error in `load_image_file()`:
      ! Unsupported image format: "gif"
      i Supported formats: PNG, JPEG, BMP, TIFF, SVG

# image_to_grayscale() rejects an array that is not an image

    Code
      image_to_grayscale(array(0, dim = c(2, 2, 3, 2)))
    Condition
      Error in `image_to_grayscale()`:
      ! Unexpected image dimensions: "2 x 2 x 3 x 2"
      i Expected a 2D or 3D array.

# extract_points_from_image() says so when no pixel passes the threshold

    Code
      extract_points_from_image(matrix(1, 3, 4), "dark", 0.5, invert_y = TRUE)
    Condition
      Error in `extract_points_from_image()`:
      ! No points found with threshold 0.5 and mode "dark".
      i Try adjusting the `threshold` value.
      i For "dark" mode, pixels below threshold are selected.
      i For "light" mode, pixels above threshold are selected.

# downsample_points() reports what it did when verbose

    Code
      kept <- downsample_points(points, max_points = NULL, verbose = TRUE)
    Message
      i Using 100 points from image.
    Code
      fewer <- downsample_points(points, max_points = 25, verbose = TRUE)
    Message
      i Downsampling from 100 to 25 points.

# surreal_image() rejects arguments it cannot use

    Code
      surreal_image(c("a.png", "b.png"))
    Condition
      Error in `surreal_image()`:
      ! `image_path` must be a single character string.
    Code
      surreal_image(path, threshold = 2)
    Condition
      Error in `surreal_image()`:
      ! `threshold` must be a numeric value between 0 and 1, or NULL for auto-detection (got 2).
    Code
      surreal_image(path, max_points = 0)
    Condition
      Error in `surreal_image()`:
      ! `max_points` must be a positive integer, Inf, or NULL for auto-detection (got 0).

