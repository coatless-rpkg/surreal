# surreal_path() rejects data it cannot use

    Code
      surreal_path(as.matrix(hidden))
    Condition
      Error in `surreal_path()`:
      ! `data` must be a data frame.
      i Use the data that `surreal()`, `surreal_text()` or `surreal_image()` returns.
    Code
      surreal_path(hidden["y"])
    Condition
      Error in `surreal_path()`:
      ! `data` must have a response named y and at least one predictor, all numeric.
      i Use the data that `surreal()`, `surreal_text()` or `surreal_image()` returns.

# print() names the best step and what is in its model

    Code
      print(path)
    Output
      <surreal_path>
      Forward selection over 25 predictors by BIC 
      BIC is lowest at step 5 
      In the model at that step: X.5, X.4, X.3, X.2, X.1 

# plot() rejects a step that is not on the path

    Code
      plot(path, step = 40)
    Condition
      Error in `plot()`:
      ! `step` must be a whole number from 0 to 25 (got 40).

