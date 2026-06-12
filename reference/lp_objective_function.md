# Set a General Nonlinear Objective Function

Optimize an R function.

## Usage

``` r
lp_minimize_function(.problem, fun, gradient = NULL, hessian = NULL)

lp_maximize_function(.problem, fun, gradient = NULL, hessian = NULL)

lp_min_fun(.problem, fun, gradient = NULL, hessian = NULL)

lp_max_fun(.problem, fun, gradient = NULL, hessian = NULL)
```

## Arguments

- .problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

- fun:

  Function to optimize. The function's arguments must match all defined
  [variables](https://benet1one.github.io/lpsugar/reference/lp_variable.md).

- gradient:

  Function that returns the gradient vector.

- hessian:

  Function that returns the hessian matrix.

## Value

An
[`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)
with the new `$objective` function.

## See also

[`lp_minimize()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md)
to optimize linear or quadratic functions.

## Examples

``` r
library(ROI.plugin.nloptr)

## Simple Example -----------------------
# max  sqrt(x) * log(y)
#  st  x + y <= 10

p1 <- lp_problem() |> 
    lp_variable(x, lower = 0) |> 
    lp_variable(y, lower = 0) |> 
    lp_maximize_function(\(x, y) sqrt(x) * log(y)) |> 
    lp_constraint(x + y <= 10)

# There are some different solvers within `nloptr`
lpsugar_applicable_solvers(p1)
#> [1] "nloptr.cobyla" "nloptr.mma"    "nloptr.auglag" "nloptr.isres" 
#> [5] "nloptr.slsqp" 
lp_solve(p1, solver = "nloptr.cobyla", start = list(x = 1, y = 1))
#> – $variables
#> $x
#> [1] 4.580018
#> 
#> $y
#> [1] 5.419982
#> 
#> – $objective
#> [1] 3.616963
#> 
#> – $status
#> Optimal Solution Found ✔ 
#> 

# See more examples in the Nonlinear vignette
# vignette("nonlinear", package = "lpsugar")
```
