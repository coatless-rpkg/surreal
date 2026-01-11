#' Check if a string is a URL
#'
#' @param x Character string to check
#'
#' @return Logical
#'
#' @noRd
is_url <- function(x) {

  grepl("^https?://", x, ignore.case = TRUE)
}

#' Download a URL to a temporary file
#'
#' @param url URL to download
#'
#' @return Path to the temporary file
#'
#' @noRd
download_to_temp <- function(url) {
  ext <- tolower(tools::file_ext(url))
  # Handle URLs with query strings

ext <- sub("\\?.*$", "", ext)
  if (!nzchar(ext)) ext <- "png"

  temp_file <- tempfile(fileext = paste0(".", ext))

  tryCatch(
    {
      utils::download.file(url, temp_file, mode = "wb", quiet = TRUE)
    },
    error = function(e) {
      cli::cli_abort(c(
        "Failed to download image from URL: {.url {url}}",
        "x" = conditionMessage(e)
      ))
    }
  )

  temp_file
}

#' Load an image file in various formats
#'
#' @param image_path Path to the image file or URL
#'
#' @return A numeric array (2D for grayscale, 3D for RGB/RGBA)
#'
#' @noRd
load_image_file <- function(image_path) {
  # Handle URLs by downloading to temp file
 if (is_url(image_path)) {
    image_path <- download_to_temp(image_path)
    on.exit(unlink(image_path), add = TRUE)
  } else if (!file.exists(image_path)) {
    cli::cli_abort(c(
      "Image file not found: {.file {image_path}}",
      "i" = "Check that the file path is correct."
    ))
  }

  ext <- tolower(tools::file_ext(image_path))
  # Handle URLs with query strings
  ext <- sub("\\?.*$", "", ext)

  image <- switch(ext,
    "png" = {
      png::readPNG(image_path)
    },
    "jpg" = ,
    "jpeg" = {
      require_packages("jpeg")
      jpeg::readJPEG(image_path)
    },
    "bmp" = {
      require_packages("bmp")
      bmp::read.bmp(image_path)
    },
    "tif" = ,
    "tiff" = {
      require_packages("tiff")
      tiff::readTIFF(image_path)
    },
    "svg" = {
      require_packages("rsvg")
      # Render SVG to bitmap (returns raw array)
      rsvg::rsvg(image_path) / 255
    },
    cli::cli_abort(c(
      "Unsupported image format: {.val {ext}}",
      "i" = "Supported formats: PNG, JPEG, BMP, TIFF, SVG"
    ))
  )

  image
}

#' Convert image array to grayscale
#'
#' @param image Numeric array from image loading
#'
#' @return 2D numeric matrix with values 0-1
#'
#' @noRd
image_to_grayscale <- function(image) {
  if (length(dim(image)) == 2) {
    # Already grayscale
    return(image)
  }

  if (length(dim(image)) == 3) {
    n_channels <- dim(image)[3]

    if (n_channels >= 3) {
      # RGB or RGBA - use luminance formula (ITU-R BT.601)
      gray <- 0.299 * image[, , 1] + 0.587 * image[, , 2] + 0.114 * image[, , 3]
    } else {
      # Single channel or grayscale with alpha
      gray <- image[, , 1]
    }
    return(gray)
  }

  cli::cli_abort(c(
    "Unexpected image dimensions: {.val {paste(dim(image), collapse = ' x ')}}",
    "i" = "Expected a 2D or 3D array."
  ))
}

#' Auto-detect mode (dark or light) based on image histogram
#'
#' @param gray_image 2D grayscale matrix (values 0-1)
#'
#' @return Character: "dark" or "light"
#'
#' @noRd
auto_detect_mode <- function(gray_image) {
  median_val <- stats::median(as.vector(gray_image), na.rm = TRUE)
  # If image is mostly light (median > 0.5), subject is likely dark
  if (median_val > 0.5) "dark" else "light"
}

#' Calculate optimal threshold using Otsu's method
#'
#' Finds the threshold that maximizes between-class variance,
#' effectively separating foreground from background.
#'
#' @param gray_image 2D grayscale matrix (values 0-1)
#'
#' @return Numeric threshold value between 0 and 1
#'
#' @noRd
otsu_threshold <- function(gray_image) {
  vals <- as.vector(gray_image)

  # Create histogram with 256 bins
  breaks <- seq(0, 1, length.out = 257)
  h <- graphics::hist(vals, breaks = breaks, plot = FALSE)
  counts <- h$counts
  mids <- h$mids

  total <- sum(counts)
  sum_all <- sum(mids * counts)

  sum_bg <- 0
  weight_bg <- 0
 max_variance <- 0
  best_threshold <- 0.5

  for (i in seq_along(counts)) {
    weight_bg <- weight_bg + counts[i]
    if (weight_bg == 0) next

    weight_fg <- total - weight_bg
    if (weight_fg == 0) break

    sum_bg <- sum_bg + mids[i] * counts[i]

    mean_bg <- sum_bg / weight_bg
    mean_fg <- (sum_all - sum_bg) / weight_fg

    # Between-class variance
    variance <- weight_bg * weight_fg * (mean_bg - mean_fg)^2

    if (variance > max_variance) {
      max_variance <- variance
      best_threshold <- mids[i]
    }
  }

  best_threshold
}

#' Auto-estimate max_points based on image size
#'
#' @param n_extracted Number of points extracted from image
#' @param img_area Total image area (width * height)
#'
#' @return Integer max_points or NULL if no downsampling needed
#'
#' @noRd
auto_max_points <- function(n_extracted, img_area) {
  # Target: 2000-5000 points for good quality without being too slow
  target <- min(5000L, max(2000L, as.integer(sqrt(img_area) * 5)))

  # Only downsample if needed
  if (n_extracted <= target) NULL else target
}

#' Extract x,y coordinates from grayscale image based on threshold
#'
#' @param gray_image 2D grayscale matrix (values 0-1)
#' @param mode "dark" or "light"
#' @param threshold Numeric threshold value (0-1)
#' @param invert_y Flip y coordinates
#'
#' @return List with x and y coordinate vectors
#'
#' @noRd
extract_points_from_image <- function(gray_image, mode, threshold, invert_y) {
  if (mode == "dark") {
    activated <- which(gray_image < threshold, arr.ind = TRUE)
  } else {
    activated <- which(gray_image > threshold, arr.ind = TRUE)
  }

  if (nrow(activated) == 0) {
    cli::cli_abort(c(
      "No points found with threshold {.val {threshold}} and mode {.val {mode}}.",
      "i" = "Try adjusting the {.arg threshold} value.",
      "i" = "For {.val dark} mode, pixels below threshold are selected.",
      "i" = "For {.val light} mode, pixels above threshold are selected."
    ))
  }

  x <- activated[, 2]
  y <- if (invert_y) {
    nrow(gray_image) - activated[, 1] + 1
  } else {
    activated[, 1]
  }

  list(x = x, y = y)
}

#' Downsample points if they exceed max_points
#'
#' @param coords List with x and y vectors
#' @param max_points Maximum number of points
#' @param verbose Print info about downsampling
#'
#' @return List with (potentially downsampled) x and y vectors
#'
#' @noRd
downsample_points <- function(coords, max_points, verbose = FALSE) {
  n_points <- length(coords$x)

  if (is.null(max_points) || n_points <= max_points) {
    if (verbose) {
      cli::cli_alert_info("Using {.val {n_points}} points from image.")
    }
    return(coords)
  }

  if (verbose) {
    cli::cli_alert_info(
      "Downsampling from {.val {n_points}} to {.val {max_points}} points."
    )
  }

  idx <- sample(seq_len(n_points), max_points)

  list(
    x = coords$x[idx],
    y = coords$y[idx]
  )
}

#' Apply the surreal method to an image file
#'
#' This function loads an image file, extracts pixel coordinates based on
#' a brightness threshold, and applies the surreal method to create a dataset
#' where the image appears in the residual plot.
#'
#' @param image_path Character. Path to an image file or a URL (PNG, JPEG, BMP, TIFF, or SVG).
#' @param mode Character. Either `"auto"` (default) to automatically detect,
#'   `"dark"` to select dark pixels, or `"light"` to select light pixels.
#' @param threshold Numeric or `NULL`. Value between 0 and 1 for grayscale
#'   threshold. If `NULL` (default), automatically calculated using Otsu's method.
#'   For `"dark"` mode, pixels below threshold are selected.
#'   For `"light"` mode, pixels above threshold are selected.
#' @param max_points Integer or `NULL`. Maximum number of points to use. If
#'   `NULL` (default), automatically estimated based on image size (typically
#'   2000-5000 points). Set to `Inf` to use all points without downsampling.
#' @param invert_y Logical. If `TRUE`, flip y-coordinates so image appears
#'   right-side up in residual plot. Default is `TRUE`.
#' @inheritParams surreal
#'
#' @return A `data.frame` containing the results of the surreal method
#'   application with columns `y`, `X1`, `X2`, ..., `Xp`.
#'
#' @details
#' By default, all parameters are automatically detected:
#' - **mode**: Detected from image histogram (dark subject on light background or vice versa)
#' - **threshold**: Calculated using Otsu's method to optimally separate foreground/background
#' - **max_points**: Estimated based on image dimensions (2000-5000 points)
#'
#' You can override any of these by specifying explicit values.
#'
#' **Input Support:**
#' - Local file paths
#' - URLs (http:// or https://) - images are downloaded to a temporary file
#'
#' **Format Support:**
#' - PNG: Supported via the `png` package (included)
#' - JPEG: Requires the `jpeg` package
#' - BMP: Requires the `bmp` package
#' - TIFF: Requires the `tiff` package
#' - SVG: Requires the `rsvg` package (renders vector graphics to bitmap)
#'
#' @examples
#' \dontrun{
#' # Simplest usage - everything auto-detected
#' result <- surreal_image("https://www.r-project.org/logo/Rlogo.png")
#' model <- lm(y ~ ., data = result)
#' plot(model$fitted, model$residuals, pch = 16)
#'
#' # Override specific parameters
#' result <- surreal_image("image.png", mode = "dark", threshold = 0.3)
#'
#' # Use all points (no downsampling)
#' result <- surreal_image("image.png", max_points = Inf)
#' }
#'
#' @seealso
#' [surreal()] for details on the surreal method parameters.
#' [surreal_text()] for embedding text instead of images.
#'
#' @export
surreal_image <- function(
    image_path,
    mode = "auto",
    threshold = NULL,
    max_points = NULL,
    invert_y = TRUE,
    R_squared = 0.3,
    p = 5,
    n_add_points = 40,
    max_iter = 100,
    tolerance = 0.01,
    verbose = FALSE) {

  mode <- match.arg(mode, c("auto", "dark", "light"))

  if (!is.character(image_path) || length(image_path) != 1) {
    cli::cli_abort("{.arg image_path} must be a single character string.")
  }

  if (!is.null(threshold)) {
    if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
      cli::cli_abort(
        "{.arg threshold} must be a numeric value between 0 and 1, or NULL for auto-detection (got {.val {threshold}})."
      )
    }
  }

  # Handle max_points: NULL = auto, Inf = no limit, positive integer = explicit
  user_max_points <- max_points
  if (!is.null(max_points) && !is.infinite(max_points)) {
    if (!is.numeric(max_points) || max_points < 1) {
      cli::cli_abort(
        "{.arg max_points} must be a positive integer, Inf, or NULL for auto-detection (got {.val {max_points}})."
      )
    }
    max_points <- as.integer(max_points)
  }

  # Step 1: Load the image
  if (verbose) cli::cli_alert_info("Loading image: {.file {image_path}}")
  image <- load_image_file(image_path)

  # Step 2: Convert to grayscale
  if (verbose) cli::cli_alert_info("Converting to grayscale.")
  gray <- image_to_grayscale(image)

  if (verbose) {
    cli::cli_alert_info(
      "Image dimensions: {.val {nrow(gray)}} x {.val {ncol(gray)}}"
    )
  }

  # Step 3: Auto-detect mode if needed
  if (mode == "auto") {
    mode <- auto_detect_mode(gray)
    if (verbose) {
      cli::cli_alert_success("Auto-detected mode: {.val {mode}}")
    }
  }

  # Step 4: Auto-detect threshold if needed
  if (is.null(threshold)) {
    threshold <- otsu_threshold(gray)
    if (verbose) {
      cli::cli_alert_success(
        "Auto-detected threshold (Otsu): {.val {round(threshold, 3)}}"
      )
    }
  }

  # Step 5: Extract coordinates based on threshold and mode
  if (verbose) {
    cli::cli_alert_info(
      "Extracting points with mode = {.val {mode}}, threshold = {.val {round(threshold, 3)}}."
    )
  }
  coords <- extract_points_from_image(gray, mode, threshold, invert_y)

  # Step 6: Auto-estimate max_points if needed
  if (is.null(user_max_points)) {
    img_area <- nrow(gray) * ncol(gray)
    max_points <- auto_max_points(length(coords$x), img_area)
    if (verbose && !is.null(max_points)) {
      cli::cli_alert_success("Auto-estimated max_points: {.val {max_points}}")
    }
  } else if (is.infinite(user_max_points)) {
    max_points <- NULL
  }

  # Step 7: Downsample if necessary
  coords <- downsample_points(coords, max_points, verbose)

  # Step 8: Apply the surreal method
  if (verbose) cli::cli_alert_info("Applying surreal method.")
  result <- surreal(
    y_hat = coords$x,
    R_0 = coords$y,
    R_squared = R_squared,
    p = p,
    n_add_points = n_add_points,
    max_iter = max_iter,
    tolerance = tolerance,
    verbose = verbose
  )

  result
}
