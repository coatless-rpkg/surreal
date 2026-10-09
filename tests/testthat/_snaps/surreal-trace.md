# surreal_trace() rejects settings outside their ranges

    Code
      surreal_trace(r_logo_image_data, step = 0)
    Condition
      Error in `surreal_trace()`:
      ! `step` must be a number above 0 and no more than 1 (got 0).
    Code
      surreal_trace(r_logo_image_data, step = 1.5)
    Condition
      Error in `surreal_trace()`:
      ! `step` must be a number above 0 and no more than 1 (got 1.5).
    Code
      surreal_trace(r_logo_image_data, R_squared = 1)
    Condition
      Error in `surreal_trace()`:
      ! `R_squared` must be between 0 and 1 (supplied: 1)
    Code
      surreal_trace(y_hat = 1:3, R_0 = 1:4)
    Condition
      Error in `surreal_trace()`:
      ! `y_hat` and `R_0` must have the same length. (3!= 4)

# plot() rejects an iteration that was not run

    Code
      plot(trace, iteration = 99)
    Condition
      Error in `plot()`:
      ! `iteration` must be a whole number from 1 to 4 (got 99).

