app_dir <- function() {
  system.file("surreal-app", package = "surreal")
}

# The app's own objects, as `app.R` defines them.
load_app <- function() {
  app <- new.env(parent = globalenv())
  sys.source(file.path(app_dir(), "app.R"), envir = app)
  app
}
