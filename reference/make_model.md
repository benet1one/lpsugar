# Make an Optimization Problem

Translate an
[`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)
object to a [`ROI::OP()`](https://rdrr.io/pkg/ROI/man/OP.html) object.
Used internally in
[`lp_solve()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md).

## Usage

``` r
make_model(problem)
```

## Arguments

- problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

## Value

An `OP` object as returned from
[`ROI::OP()`](https://rdrr.io/pkg/ROI/man/OP.html).

## Examples

``` r
library(ROI) |> suppressMessages()

p <- lp_problem() |>
    lp_var(x[a = 1:2, b = 1:2], upper = 10) |>
    lp_alias(sum_x = sum(x)) |>
    lp_max(sum_x + 5)

# Solve directly with lp_solve()
direct_solution <- lp_solve(p)
direct_solution
#> $variables
#> $variables$x
#>    b
#> a    1  2
#>   1 10 10
#>   2 10 10
#> 
#> 
#> $aliases
#> $aliases$sum_x
#> [1] 40
#> 
#> 
#> $objective
#> [1] 45
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

# Or do each step
model <- make_model(p)
solution_raw <- solve_model(model)
solution_pretty <- pretty_solution(problem = p, solution = solution_raw)

# pretty_solution() gives each variable its dim and dimnames.
solution_raw$solution
#> x[1,1] x[2,1] x[1,2] x[2,2] 
#>     10     10     10     10 
solution_pretty$variables
#> $x
#>    b
#> a    1  2
#>   1 10 10
#>   2 10 10
#> 

# It also computes aliases
solution_pretty$aliases
#> $sum_x
#> [1] 40
#> 

# Notice that in solution_raw, the objective value is 40 instead of 45.
# The objective function is (sum(x) + 5), but the addend (+5) is irrelevant
# for finding the optimum, so ROI ignores it.
solution_raw$objval
#> [1] 40
solution_pretty$objective
#> [1] 45
```
