# Nonlinear Expression

Nonlinear transformations and operations on variables.

## Usage

``` r
nonlinear(expr)

nl(expr)
```

## Arguments

- expr:

  (Unquoted) expression containing decision variables. Must return a
  numeric vector when evaluated.

## Value

The quoted expression with class `"nonlinear"`

## Examples

``` r
# max  sqrt(x) * log(y)
#  st  x^y <= 10

p <- lp_problem() |> 
    lp_variable(x, lower = 0) |> 
    lp_variable(y, lower = 1) |> 
    lp_maximize(nonlinear(sqrt(x) * log(y))) |> 
    lp_constraint(nonlinear(x^y) <= 5)

print(p)
#> –– <lp_problem> ––
#> 
#> – $variables
#> $x
#> Real scalar 'x'
#> x >= 0
#> 
#> $y
#> Real scalar 'y'
#> y >= 1
#> 
#> – $objective
#> maximize nonlinear function:
#> sqrt(x) * log(y)
#> 
#> – $constraints
#> An object containing 1 nonlinear constraint.
#> 
#> #unnamed_constraint
#> | nonlinear(x^y) <= 5
#> | Rows = 1
#> 

library(ROI.plugin.nloptr)
s <- lp_solve(p, solver = "nloptr.cobyla", start = list(x = 2, y = 2))
print(s)
#> – $variables
#> $x
#> [1] 1.083966
#> 
#> $y
#> [1] 19.93128
#> 
#> – $objective
#> [1] 3.115384
#> 
#> – $status
#> Optimal Solution Found ✔ 
#> 
```
