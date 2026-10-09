# Drawing code for the app's plots. Each plot is drawn the same way on screen
# and in its download, apart from size: `full = TRUE` is the download, with
# axis titles and more room.

# The colors the plots use in light and in dark mode
plot_palette <- function(is_dark) {
  if (is_dark) {
    list(
      bg = "#1a202c",
      fg = "#f7fafc",
      point = "#63b3ed",
      rule = "#4a5568",
      mark = "#ed8936"
    )
  } else {
    list(
      bg = "#ffffff",
      fg = "#2d3748",
      point = "#1a365d",
      rule = "#cbd5e0",
      mark = "#ed8936"
    )
  }
}

# A scatterplot in the app's colors, with axis titles when `labels` is given
draw_points <- function(x, y, palette, point_size, labels = NULL, asp = NA) {
  compact <- is.null(labels)

  par(
    mar = if (compact) c(3, 3, 1, 1) else c(4, 4, 1, 1),
    bg = palette$bg,
    fg = palette$fg,
    col.axis = palette$fg,
    col.lab = palette$fg
  )
  # With no predictors in a model every fitted value is the same, give or
  # take rounding. The axis then gets a unit of room on each side.
  xlim <- range(x)
  if (diff(xlim) < 1e-8 * max(1, abs(xlim))) {
    xlim <- mean(xlim) + c(-1, 1)
  }

  plot(
    x,
    y,
    pch = 20,
    cex = if (compact) point_size * 0.8 else point_size,
    col = palette$point,
    xlab = if (compact) "" else labels[1],
    ylab = if (compact) "" else labels[2],
    axes = FALSE,
    asp = asp,
    xlim = xlim
  )
  axis(1, cex.axis = if (compact) 0.8 else 1)
  axis(2, cex.axis = if (compact) 0.8 else 1)
  box()
}

# Fitted values against residuals, where the hidden image shows
draw_residuals <- function(model, palette, point_size, full = FALSE) {
  draw_points(
    model$fitted.values,
    model$residuals,
    palette,
    point_size,
    labels = if (full) c("Fitted", "Residuals")
  )
  abline(h = 0, lty = 2, col = palette$rule)
}

# What the data was made from. `source` is a list with a `type` ("image",
# "text" or "demo"), the `coords` (a file path for an image, x and y for a
# demo) and the `text`. Before anything is generated the plot is left empty.
draw_source <- function(source, palette, point_size, full = FALSE) {
  if (identical(source$type, "demo")) {
    draw_points(
      source$coords$x,
      source$coords$y,
      palette,
      point_size,
      labels = if (full) c("X", "Y"),
      asp = 1
    )
    return(invisible())
  }

  par(mar = c(0, 0, 0, 0), bg = palette$bg, fg = palette$fg)

  if (is.null(source$type)) {
    plot.new()
  } else if (source$type == "image") {
    img <- surreal:::load_image_file(source$coords)
    plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1)
    graphics::rasterImage(img, 0, 0, 1, 1)
  } else if (source$type == "text") {
    plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(
      0.5,
      0.5,
      source$text,
      cex = if (full) 4 else 3,
      col = palette$point,
      font = 2
    )
  }
}

# Scatterplot matrix of the generated data, with see-through points
draw_pairs <- function(data, palette) {
  par(
    bg = palette$bg,
    fg = palette$fg,
    col.axis = palette$fg,
    col.lab = palette$fg
  )
  pairs(data, pch = 20, cex = 0.3, col = paste0(palette$point, "60"))
}

# One frame of a recorded search or selection: the residual plot as it stood
draw_frame <- function(fitted, residuals, palette, point_size) {
  draw_residuals(
    list(fitted.values = fitted, residuals = residuals),
    palette,
    point_size
  )
}

# Numbers for an axis, written in full: 0.01 and 48,500, not 1e-02 and 48500
axis_labels <- function(at) {
  format(
    at,
    big.mark = ",",
    scientific = FALSE,
    drop0trailing = TRUE,
    trim = TRUE
  )
}

# The plotting area for a quantity that runs along the steps, with the step on
# screen marked in color and the best step, when there is one, dashed. The
# labels on the y axis read across, so long numbers stay legible.
draw_along <- function(x, y, at, palette, xlab, ylab, best = NULL, ...) {
  par(
    mar = c(3.5, 5.6, 1, 1),
    mgp = c(2.2, 0.7, 0),
    bg = palette$bg,
    fg = palette$fg,
    col.axis = palette$fg,
    col.lab = palette$fg
  )
  matplot(x, y, xlab = xlab, ylab = "", axes = FALSE, ...)
  # Steps are whole numbers, so the axis is labeled at whole numbers only
  axis(1, at = unique(round(axTicks(1))), cex.axis = 0.8)
  ticks <- axTicks(2)
  axis(2, at = ticks, labels = axis_labels(ticks), las = 1, cex.axis = 0.8)
  title(ylab = ylab, line = 4.4)
  box()
  if (!is.null(best)) {
    abline(v = best, lty = 2, col = palette$fg)
  }
  abline(v = at, col = palette$mark, lwd = 2)
}

# How far the fitted values were from their targets at each iteration
draw_search_path <- function(trace, iteration, palette) {
  draw_along(
    trace$iterations$iteration,
    trace$iterations$distance,
    at = iteration,
    palette = palette,
    xlab = "Iteration",
    ylab = "Distance (log scale)",
    type = "o",
    pch = 20,
    lty = 1,
    col = palette$point,
    log = "y"
  )
}

# The coefficient of every predictor along a selection, decoys in gray
draw_coefficient_paths <- function(path, step, palette) {
  slopes <- path$coefficients[, -1, drop = FALSE]
  draw_along(
    path$steps$step,
    slopes,
    at = step,
    palette = palette,
    xlab = "Step",
    ylab = "Coefficient",
    best = path$best,
    type = "s",
    lty = 1,
    col = ifelse(colnames(slopes) %in% path$decoys, palette$rule, palette$point)
  )
}

# The criterion along a selection
draw_criterion <- function(path, step, palette) {
  draw_along(
    path$steps$step,
    path$steps$criterion,
    at = step,
    palette = palette,
    xlab = "Step",
    ylab = path$criterion,
    best = path$best,
    type = "l",
    lty = 1,
    col = palette$point
  )
}
