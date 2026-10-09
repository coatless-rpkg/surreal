# Apply the surreal method to a text string

This function applies the surreal method to a text string. It first
finds the points that draw the text with
[`surreal_text_points()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text_points.md),
and then applies the surreal method to them.

## Usage

``` r
surreal_text(
  text = "hello world",
  cex = 4,
  R_squared = 0.3,
  p = 5,
  n_add_points = 40,
  max_iter = 100,
  tolerance = 0.01,
  verbose = FALSE
)
```

## Arguments

- text:

  Character. A plain text message to be plotted. Default is "hello
  world".

- cex:

  Numeric. A value specifying the relative size of the text. Default is
  4.

- R_squared:

  Numeric. Desired R-squared value. Default is 0.3.

- p:

  Integer. Desired number of columns for matrix X. Default is 5.

- n_add_points:

  Integer. Number of points to add in border transformation. Default is
  40.

- max_iter:

  Integer. Maximum number of iterations for convergence. Default is 100.

- tolerance:

  Numeric. Criteria for detecting convergence and stopping optimization
  early. Default is 0.01.

- verbose:

  Logical. If TRUE, prints progress information. Default is FALSE.

## Value

A data.frame containing the results of the surreal method application.

## See also

[`surreal()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal.md)
for details on the surreal method parameters.
[`surreal_text_points()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text_points.md)
for the points of the text on their own.

## Examples

``` r
# Create a surreal plot of the text "R is fun" appearing on one line
r_is_fun_result <- surreal_text("R is fun", verbose = TRUE)

#> Optimal alpha: 1.49389 
#> Iteration 1 - Delta: 150329.4 
#> Iteration 2 - Delta: 1.616536 
#> Iteration 3 - Delta: 0.001207269 



# Create a surreal plot of the text "Statistics Rocks" by using an escape
# character to create a second line between "Statistics" and "Rocks"
stat_rocks_result <- surreal_text("Statistics\nRocks", verbose = TRUE)

#> Optimal alpha: 1.339718 
#> Iteration 1 - Delta: 2561707 
#> Iteration 2 - Delta: 100.7154 
#> Iteration 3 - Delta: 0.03851556 
#> Iteration 4 - Delta: 0.0001190154 


```
