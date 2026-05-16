# Compute a Summary of a Solution or Point

Check feasibility, constraint saturation, calculate objective value and
aliases.

## Usage

``` r
solution_summary(problem, solution, tol = 2e-06)

constraint_summary(problem, solution, tol = 2e-06)

bound_summary(problem, solution, tol = 2e-06)

compute_objective(problem, solution)

compute_aliases(problem, solution)
```

## Arguments

- problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

- solution:

  One of:

  - Named list of variables with their respective values. If a variable
    is missing, it is set to `pmax(0, lower)`.

  - An `lp_solution` object as returned by
    [`lp_solve()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md).

  - A vector containing the values of each variable, one after another.

- tol:

  Tolerance to use for constraint and bound satisfaction.

## Value

A named list with the computed statistics.

## Examples

``` r
problem <- lp_problem() |>
    lp_var(x, lower = 0) |>
    lp_var(y[1:3], integer = TRUE) |>
    lp_alias(sum_y = sum(y)) |>
    lp_min(x + y[1]) |>
    lp_con(
        c1 = 2*x - y[3] == 10,
        c2 = for (i in 1:3) y[i] <= i
    )

unfeasible_solution <- list(
    x = -1,
    y = c(4, 2, -3)
)

solution_summary(problem, unfeasible_solution)
#> $aliases
#> $aliases$sum_y
#> [1] 3
#> 
#> 
#> $constraints
#>   name fullname lhs dir rhs satisfied saturated
#> 1   c1       c1   1  ==  10     FALSE        NA
#> 2   c2  c2[i=1]   4  <=   1     FALSE        NA
#> 3   c2  c2[i=2]   2  <=   2      TRUE      TRUE
#> 4   c2  c2[i=3]  -3  <=   3      TRUE     FALSE
#> 
#> $bounds
#>   variable lower value upper satisfied saturated
#> 1        x     0    -1   Inf     FALSE        NA
#> 2     y[1]  -Inf     4   Inf      TRUE     FALSE
#> 3     y[2]  -Inf     2   Inf      TRUE     FALSE
#> 4     y[3]  -Inf    -3   Inf      TRUE     FALSE
#> 
#> $feasible
#> [1] FALSE
#> 
#> $objective
#> [1] 3
#> 
```
