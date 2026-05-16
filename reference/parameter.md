# Set Dimensions and Names for Parameters

Return a named vector or matrix.

## Usage

``` r
parameter(.x, ..., byrow = TRUE)
```

## Arguments

- .x:

  Vector or matrix of any type.

- ...:

  Dimnames. If you want to return a vector, it needs one element; if you
  want to return a matrix it needs two elements.

- byrow:

  Boolean, true by default. Only used if `.x` is a vector and `...` has
  two elements.

## Value

A named vector or matrix.

## Examples

``` r
my_set <- letters[1:3]
my_parameter <- c(2, 6, 3) |> parameter(my_set)
my_parameter
#> my_set
#> a b c 
#> 2 6 3 

rows <- letters[1:2]
cols <- LETTERS[1:3]
my_matrix <- c(
    1, 2, 3,
    4, 5, 6
) |> parameter(rows, cols)
my_matrix
#>     cols
#> rows A B C
#>    a 1 2 3
#>    b 4 5 6

# Also works if .x is already a matrix.
mat <- matrix(1:6, nrow = 3, ncol = 2)
mat |> parameter(r = letters[1:3], c = LETTERS[1:2])
#>    c
#> r   A B
#>   a 1 4
#>   b 2 5
#>   c 3 6
```
