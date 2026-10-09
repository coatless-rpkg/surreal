app_dir <- function() {
  system.file("surreal-app", package = "surreal")
}

# The app's own objects, as `app.R` defines them.
load_app <- function() {
  app <- new.env(parent = globalenv())
  sys.source(file.path(app_dir(), "app.R"), envir = app)
  app
}

# The inputs the app starts with, with any changed by name.
app_inputs <- function(...) {
  inputs <- list(
    input_mode = "text",
    text = "A",
    r_squared = 0.3,
    p = 5,
    point_size = 0.6,
    max_points = 3000,
    image_mode = "auto",
    threshold = 0.5,
    dark_mode = "light"
  )
  utils::modifyList(inputs, list(...))
}

# The eight bytes every PNG file starts with.
png_signature <- function() {
  as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
}
