# Define Multiple Constraints at Once

Concatenate constraints.

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
#> An object containing 9 linear constraints.
#> 
#> #unnamed_constraint
#> | sum(is_value) == 2
#> | Rows = 1
#> 
#>    y[1] y[2] y[3] y[4] is_value[1] is_value[2] is_value[3] is_value[4] dir rhs
#>    0    0    0    0    1           1           1           1           ==  2  
#> 
#> #unnamed_constraint
#> | for (i in 1:n) bind_cons(y[i] >= lower + is_value[i] * (value -  ...
#> | Rows = 8
#> 
#>         y[1] y[2] y[3] y[4] is_value[1] is_value[2] is_value[3] is_value[4] dir
#>   [i=1] 1    0    0    0    -2          0           0           0           >= 
#>   [i=1] 1    0    0    0    4           0           0           0           <= 
#>   [i=2] 0    1    0    0    0           -2          0           0           >= 
#>   [i=2] 0    1    0    0    0           4           0           0           <= 
#>   [i=3] 0    0    1    0    0           0           -2          0           >= 
#>   [i=3] 0    0    1    0    0           0           4           0           <= 
#>   [i=4] 0    0    0    1    0           0           0           -2          >= 
#>   [i=4] 0    0    0    1    0           0           0           4           <= 
#>         rhs
#>   [i=1] 1  
#>   [i=1] 7  
#>   [i=2] 1  
#>   [i=2] 7  
#>   [i=3] 1  
#>   [i=3] 7  
#>   [i=4] 1  
#>   [i=4] 7  
#> 

library(ROI.plugin.highs)
s <- lp_solve(p, binary_as_logical = TRUE)
s$variables
#> $y
#> [1] 7 7 3 3
#> 
#> $is_value
#> [1] FALSE FALSE  TRUE  TRUE
#> 
```
