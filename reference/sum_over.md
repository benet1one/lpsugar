# Index Based Summation

Sum over one or more indexing variables.

## Usage

``` r
sum_over(...)
```

## Arguments

- ...:

  The first argument(s) must be named: the name represents the indexing
  variable, and the values represent the sequence over which to sum.

  The last argument is the expression of the sum.

## Value

The value of the sum.

## Details

The syntax is similar to the one used in math. For instance, \\
\sum\_{i=1}^n \sum\_{j=1}^m {x\_{ij} \* c_j} \\ would be written as
`sum_over(i = 1:n, j = 1:n, x[i,j] * c[j])`.

## Examples

``` r
cost <- c(5, 2, 7)
p <- lp_problem() |>
  lp_variable(x[1:2, 1:3]) |>
  lp_minimize(sum_over(i = 1:2, j = 1:3, x[i, j] * cost[j]))
p$objective
#> minimize sum_over(i = 1:2, j = 1:3, x[i, j] * cost[j]) 
#> $coef
#> x[1,1] x[2,1] x[1,2] x[2,2] x[1,3] x[2,3] 
#>      5      5      2      2      7      7 
#> 
```
