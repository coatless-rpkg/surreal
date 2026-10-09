# Drawing code for the app's plots. Each plot is drawn the same way on screen
# and in its download, apart from size: `full = TRUE` is the download, with
# axis titles and more room.

# The colors the plots use in light and in dark mode
plot_palette <- function(is_dark) {
  if (is_dark) {
    list(bg = "#1a202c", fg = "#f7fafc", point = "#63b3ed", rule = "#4a5568")
  } else {
    list(bg = "#ffffff", fg = "#2d3748", point = "#1a365d", rule = "#cbd5e0")
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
  plot(
    x,
    y,
    pch = 20,
    cex = if (compact) point_size * 0.8 else point_size,
    col = palette$point,
    xlab = if (compact) "" else labels[1],
    ylab = if (compact) "" else labels[2],
    axes = FALSE,
    asp = asp
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
