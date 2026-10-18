# Index Based Summation

Sum over one or more indexing variables.

## Usage

``` r
sum_over(.expr, ...)
```

## Arguments

- .expr:

  Values to sum.

- ...:

  Indices and filters.

  - Named arguments are indices, `<name> = <vector>`.

  - Unnamed arguments are filters on the indices.

## Value

The value of the sum.

## Details

The syntax is similar to the one used in math. For instance,

` ` \\ \sum\_{i=1}^n \sum\_{j=i}^m {x\_{ij} \* a_j} \\

would be written as

` ` `sum_over(x[i,j] * a[j], i = 1:n, j = i:m)`

## Examples

``` r
a <- c(5, 2, 7)
p <- lp_problem() |>
  lp_variable(x[1:2, 1:3]) |>
  lp_minimize(sum_over(x[i, j] * a[j], i = 1:2, j = i:3))
```
