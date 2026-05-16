# Bind constraints

Define multiple constraints at once.

## Usage

``` r
bind_cons(...)
```

## Arguments

- ...:

  Constraints. See
  [`lp_constraint()`](https://benet1one.github.io/lpsugar/reference/lp_constraint.md).

## Examples

``` r
n <- 4

lower <- 1
value <- 3
upper <- 7

p <- lp_problem() |>
    lp_var(y[1:n], lower = lower, upper = upper) |>
    lp_var(is_value[1:n], binary = TRUE) |>
    lp_max(y[1] + y[2] - y[3] - y[4]) |>
    lp_con(
        sum(is_value) == 2,
        for (i in 1:n) bind_cons(
            y[i] >= lower + is_value[i] * (value - lower),
            y[i] <= upper - is_value[i] * (upper - value)
        )
    )

p$constraints
#> 
#>  <unnamed> | n = 1 | sum(is_value) == 2
#> 
#>  y[1] y[2] y[3] y[4] is_value[1] is_value[2] is_value[3] is_value[4] dir  
#>  0    0    0    0    1           1           1           1           ==  2
#> 
#> 
#>  <unnamed> | n = 8 | for (i in 1:n) bind_cons(y[i] >= lower + is_value[i] * (value -  ...
#> 
#>       y[1] y[2] y[3] y[4] is_value[1] is_value[2] is_value[3] is_value[4] dir  
#> [i=1] 1    0    0    0    -2          0           0           0           >=  1
#> [i=1] 1    0    0    0    4           0           0           0           <=  7
#> [i=2] 0    1    0    0    0           -2          0           0           >=  1
#> [i=2] 0    1    0    0    0           4           0           0           <=  7
#> [i=3] 0    0    1    0    0           0           -2          0           >=  1
#> [i=3] 0    0    1    0    0           0           4           0           <=  7
#> [i=4] 0    0    0    1    0           0           0           -2          >=  1
#> [i=4] 0    0    0    1    0           0           0           4           <=  7
#> 

library(ROI)
#> ROI: R Optimization Infrastructure
#> Registered solver plugins: nlminb, highs.
#> Default solver: auto.
s <- lp_solve(p, binary_as_logical = TRUE)
s$variables
#> $y
#> 1:n
#> 1 2 3 4 
#> 7 7 3 3 
#> 
#> $is_value
#> 1:n
#>     1     2     3     4 
#> FALSE FALSE  TRUE  TRUE 
#> 
```
