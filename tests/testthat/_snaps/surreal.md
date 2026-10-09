# surreal() reproduces its recorded data for a fixed seed

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["y", "X.1", "X.2"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {},
          "value": [0.595694, -0.98405, 0.781309, 2.854105, 0.168455, 2.026352, -0.323754, 1.664475, 2.790055, 1.428192, 1.072817, 2.86357]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.336744, -1.374108, -0.184424, -0.454166, -0.349989, -0.353826, 0.207153, 0.509565, 0.24997, 0.339012, 0.189584, -1.100131]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.862637, 0.598855, -0.269797, 0.068011, 0.030491, 0.142754, -0.298674, -0.426084, -0.116616, -0.131596, 0.052792, 1.239178]
        }
      ]
    }

# surreal() rejects settings outside their ranges

    Code
      surreal(picture, R_squared = 1)
    Condition
      Error in `surreal()`:
      ! `R_squared` must be between 0 and 1 (supplied: 1)
    Code
      surreal(picture, p = 0)
    Condition
      Error in `surreal()`:
      ! `p` must be at least 1 (supplied: 0)
    Code
      surreal(picture, n_add_points = -1)
    Condition
      Error in `surreal()`:
      ! `n_add_points` must be a non-negative integer (supplied: -1)
    Code
      surreal(picture, max_iter = 0)
    Condition
      Error in `surreal()`:
      ! `max_iter` must be at least 1 (supplied: 0)
    Code
      surreal(picture, tolerance = 0)
    Condition
      Error in `surreal()`:
      ! `tolerance` must be a positive number (supplied: 0)
    Code
      surreal(y_hat = 1:3, R_0 = 1:4)
    Condition
      Error in `surreal()`:
      ! `y_hat` and `R_0` must have the same length. (3!= 4)

