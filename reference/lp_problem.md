# Create a Linear Problem

Use
[`lp_variable()`](https://benet1one.github.io/lpsugar/reference/lp_variable.md)
to define the variables,
[`lp_minimize()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md)
or
[`lp_maximize()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md)
to define the objective function,
[`lp_constraint()`](https://benet1one.github.io/lpsugar/reference/lp_constraint.md)
to add constraints and
[`lp_solve()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md)
to find the optimum.

## Usage

``` r
lp_problem()
```

## Value

An `lp_problem` object with fields:

- `$variables` : List of variables defined with
  [`lp_variable()`](https://benet1one.github.io/lpsugar/reference/lp_variable.md).

- `$objective` : List with information about the objective function, set
  with
  [`lp_minimize()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md)
  or
  [`lp_maximize()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md).

- `$constraints` : List of constraints added with
  [`lp_constraint()`](https://benet1one.github.io/lpsugar/reference/lp_constraint.md).

## Examples

``` r
# ROI or a ROI plugin needs to be loaded for lp_solve() to work
library(ROI) |> suppressMessages()

lp_problem() |>
    lp_variable(x[1:2], lower = 0) |>
    lp_maximize(x[1] + x[2]) |>
    lp_constraint(
        2*x[1] +   x[2] <= 8,
        2*x[1] + 3*x[2] <= 12
    ) |>
    lp_solve()
#> $variables
#> $variables$x
#> 1:2
#> 1 2 
#> 3 2 
#> 
#> 
#> $objective
#> [1] 5
#> 
#> $status$code = 0  (Optimal)
#> 
#> Fields:
#> -- $objective --
#> -- $variables --
#> -- $aliases --
#> -- $variables_vec --
#> -- $status --
#> -- $message --
#> -- $model --

# For a Quick Start guide see `vignette("lpsugar", package = "lpsugar")`
```
