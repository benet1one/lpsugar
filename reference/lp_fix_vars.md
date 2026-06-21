# Fix Variables to a Value

Set the lower and upper bounds of variables to a fixed value.

## Usage

``` r
lp_fix_vars(.problem, ...)
```

## Arguments

- .problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

- ...:

  Name-value pairs, variables with their respective values. `NA` values
  are not fixed, and will remain free within their bounds.

## Value

The `.problem` with modified lower and upper bounds for the variables.

## Examples

``` r
# Variable `x` has 5 values
fixed_x <- numeric(5)
# The first value of `x` should be 1, last value should be 9
fixed_x[1] <- 1
fixed_x[5] <- 9
# Other values are free
fixed_x[2:4] <- NA

p <- lp_problem() |> 
    lp_var(x[1:5], lower = 0) |> 
    lp_fix_vars(x = fixed_x)

rbind(
    lower = p$variables$x$lower,
    upper = p$variables$x$upper
)
#>       [,1] [,2] [,3] [,4] [,5]
#> lower    1    0    0    0    9
#> upper    1  Inf  Inf  Inf    9
```
