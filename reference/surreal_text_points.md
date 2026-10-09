# Turn text into the points that draw it

This function draws the text on a temporary bitmap and returns the
position of every pixel the text covers. These are the points that
[`surreal_text()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text.md)
hides. Getting them first lets you look at them, change them, or combine
them with other points before handing them to
[`surreal()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal.md).

## Usage

``` r
surreal_text_points(text = "hello world", cex = 4)
```

## Arguments

- text:

  Character. A plain text message to be plotted. Default is "hello
  world".

- cex:

  Numeric. A value specifying the relative size of the text. Default is
  4.

## Value

A data.frame with one row for each point of the text and two columns,
`x` and `y`.

## See also

[`surreal_text()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_text.md)
to go from text to a dataset in one step.
[`surreal_image_points()`](https://r-pkg.thecoatlessprofessor.com/surreal/reference/surreal_image_points.md)
for the points of an image.

## Examples

``` r
# The points that draw "R is fun"
points <- surreal_text_points("R is fun")
plot(points, pch = 16, asp = 1)


# Hide them in a dataset, as surreal_text() does
result <- surreal(points)
```
