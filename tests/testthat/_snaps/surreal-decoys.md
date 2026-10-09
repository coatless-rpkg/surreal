# surreal_decoys() rejects data and settings it cannot use

    Code
      surreal_decoys(as.matrix(hidden))
    Condition
      Error in `surreal_decoys()`:
      ! `data` must be a data frame.
      i Use the data that `surreal()`, `surreal_text()` or `surreal_image()` returns.
    Code
      surreal_decoys(hidden[-1])
    Condition
      Error in `surreal_decoys()`:
      ! `data` must have a response named y and at least one predictor, all numeric.
      i Use the data that `surreal()`, `surreal_text()` or `surreal_image()` returns.
    Code
      surreal_decoys(hidden, n = 0)
    Condition
      Error in `surreal_decoys()`:
      ! `n` must be a whole number of at least 1 (got 0).
    Code
      surreal_decoys(hidden, n = 2.5)
    Condition
      Error in `surreal_decoys()`:
      ! `n` must be a whole number of at least 1 (got 2.5).
    Code
      surreal_decoys(hidden, sd = -1)
    Condition
      Error in `surreal_decoys()`:
      ! `sd` must be a positive number, or NULL to match the predictors (got -1).

