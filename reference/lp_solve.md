# Solve a Linear Problem

`lp_solve()` computes the optimal solution of an
[`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md),
whereas `lp_find_feasible()` returns an arbitrary feasible solution.

## Usage

``` r
lp_solve(.problem, solver, binary_as_logical = FALSE, ...)

lp_find_feasible(.problem, binary_as_logical = FALSE, ...)
```

## Arguments

- .problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

- solver:

  String specifying the solver to use. If missing, then the default
  solver returned by
  [`ROI::ROI_options()`](https://rdrr.io/pkg/ROI/man/ROI_options.html)
  is used.

- binary_as_logical:

  Boolean. If `FALSE` (the default), binary variables are returned as
  `{0, 1}`. If `TRUE`, binary variables are returned as logical
  `{FALSE, TRUE}`.

- ...:

  Control arguments to be passed on to the solver.

## Examples

``` r
library(ROI) |> suppressMessages()

r <- letters[1:2]
c <- LETTERS[1:3]

# Problem with Optimal Solution
p_opt <- lp_problem() |>
    lp_variable(x[r, c], binary = TRUE) |>
    lp_alias(total = sum(x)) |>
    lp_maximize(total + 1) |>
    lp_constraint(x[1, 1] + x[2, 1] <= 1L)

s1 <- lp_solve(p_opt)
s1$status
#> $code
#> [1] 0
#> 
#> $msg
#>   solver highs
#>     code 7
#>   symbol OPTIMAL
#>  message Optimal
#> roi_code 0
#> 
s1$objective
#> [1] 6
s1$aliases$total
#> [1] 5
s1$variables$x
#>    c
#> r   A B C
#>   a 1 1 1
#>   b 0 1 1

s2 <- lp_solve(p_opt, binary_as_logical = TRUE)
s2$variables$x
#>    c
#> r       A    B    C
#>   a  TRUE TRUE TRUE
#>   b FALSE TRUE TRUE


# Problem with Unbounded Solution
p_unb <- lp_problem() |>
    lp_variable(y) |>
    lp_max(y)

s3 <- lp_solve(p_unb)
s3$status
#> $code
#> [1] 1
#> 
#> $msg
#>   solver highs
#>     code 10
#>   symbol UNBOUNDED
#>  message Unbounded
#> roi_code 1
#> 
s3$objective # Might change depending on solver
#> [1] 0

# Infeasible Problem
p_no <- lp_problem() |>
    lp_variable(z[letters]) |>
    lp_constraint(z[1] <= z[1] - 10)

s5 <- lp_find_feasible(p_no)

s5$status
#> $code
#> [1] 1
#> 
#> $msg
#>   solver highs
#>     code 8
#>   symbol INFEASIBLE
#>  message Infeasible
#> roi_code 1
#> 
s5$variables$z
#> letters
#> a b c d e f g h i j k l m n o p q r s t u v w x y z 
#> 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
s5$objective
#> [1] 0
```
