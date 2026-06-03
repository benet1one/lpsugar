# Prettify the Solution of a Model.

Takes a problem and its solution and prettifies the solution. Used
internally in
[`lp_solve()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md).

## Usage

``` r
pretty_solution(problem, solution, binary_as_logical = FALSE)
```

## Arguments

- problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

- solution:

  A list as returned by
  [`ROI::ROI_solve()`](https://rdrr.io/pkg/ROI/man/ROI_solve.html).

- binary_as_logical:

  Boolean. If `FALSE` (the default), binary variables are returned as
  `{0, 1}`. If `TRUE`, binary variables are returned as logical
  `{FALSE, TRUE}`.

## Value

A list with the following fields:

- `$objective` : Scalar, value of the objective function at optimum.

- `$variables` : List of arrays, values of the variables at optimum.

- `$aliases` : List of arrays, values of the aliases at optimum.

- `$variables_vec` : Numeric vector, values of the variables at optimum.

- `$status` : Status as returned by
  [`ROI::ROI_solve()`](https://rdrr.io/pkg/ROI/man/ROI_solve.html).

- `$message` : Message as returned by
  [`ROI::ROI_solve()`](https://rdrr.io/pkg/ROI/man/ROI_solve.html),

- `$op` : Optimization Problem `OP`, as returned by
  [`ROI::as.OP()`](https://rdrr.io/pkg/ROI/man/OP.html).

## See also

[`lp_solve()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md)
for the standard way to solve a problem.

[`as.OP.lp_problem()`](https://benet1one.github.io/lpsugar/reference/as.OP.lp_problem.md)
to convert an `lp_problem` to an Optimization Problem `(OP)` object from
package `ROI`.

## Examples

``` r
library(ROI.plugin.highs)

problem <- lp_problem() |>
    lp_var(x[a = 1:2, b = 1:2], upper = 10) |>
    lp_alias(sum_x = sum(x)) |>
    lp_max(sum_x + 5)

# Solve directly with lp_solve()
direct_solution <- lp_solve(problem)
direct_solution
#> – $variables
#> $x
#>       b
#> a      [,1] [,2]
#>   [1,]   10   10
#>   [2,]   10   10
#> 
#> – $aliases
#> $sum_x
#> [1] 40
#> 
#> – $objective
#> [1] 45
#> – $status
#> Optimal Solution Found ✔ 
#> 

# Or run each step manually
op <- as.OP(problem)
solution_raw <- ROI_solve(op)
solution_pretty <- pretty_solution(problem, solution = solution_raw)

# pretty_solution() gives each variable its dim and dimnames.
solution_raw$solution
#> x[1,1] x[2,1] x[1,2] x[2,2] 
#>     10     10     10     10 
solution_pretty$variables
#> $x
#>       b
#> a      [,1] [,2]
#>   [1,]   10   10
#>   [2,]   10   10
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
