# The R logo hidden behind five predictors, the same data every time.
hidden_logo <- function() {
  withr::with_seed(114, surreal(r_logo_image_data))
}

# The same data with twenty decoy predictors after the real ones.
decoyed_logo <- function() {
  withr::with_seed(1, surreal_decoys(hidden_logo(), n = 20, shuffle = FALSE))
}
