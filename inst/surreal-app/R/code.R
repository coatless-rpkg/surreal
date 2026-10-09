# The R code that makes the data the app is showing, for the Code dialog.
# `settings` holds the app's inputs by name.
example_code <- function(input_mode, settings) {
  switch(
    input_mode,
    "demo_jack" = 'library(surreal)
data("jackolantern_surreal_data")
result <- jackolantern_surreal_data
model <- lm(y ~ ., data = result)
plot(model$fitted.values, model$residuals, pch = 20)',

    "demo_rlogo" = sprintf(
      'library(surreal)
data("r_logo_image_data")
result <- surreal(r_logo_image_data, R_squared = %.2f, p = %d)
model <- lm(y ~ ., data = result)
plot(model$fitted.values, model$residuals, pch = 20)',
      settings$r_squared,
      settings$p
    ),

    "text" = sprintf(
      'library(surreal)
result <- surreal_text("%s", R_squared = %.2f, p = %d)
model <- lm(y ~ ., data = result)
plot(model$fitted.values, model$residuals, pch = 20)',
      settings$text,
      settings$r_squared,
      settings$p
    ),

    "image" = sprintf(
      'library(surreal)
result <- surreal_image(
  "path/to/your/image.png",
  mode = "%s",
  threshold = %s,
  max_points = %d,

  R_squared = %.2f,
  p = %d
)
model <- lm(y ~ ., data = result)
plot(model$fitted.values, model$residuals, pch = 20)',
      settings$image_mode,
      if (settings$image_mode == "auto") {
        "NULL"
      } else {
        sprintf("%.2f", settings$threshold)
      },
      settings$max_points,
      settings$r_squared,
      settings$p
    )
  )
}
