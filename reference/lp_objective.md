# Set an Objective Function

Minimize of maximize a linear or quadratic expression.

## Usage

``` r
lp_minimize(.problem, objective)

lp_maximize(.problem, objective)

lp_min(.problem, objective)

lp_max(.problem, objective)
```

## Arguments

- .problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

- objective:

  Expression to optimize. Can be:

  - The number 0, in which case the solver will attempt to find any
    feasible solution.
    [`lp_find_feasible()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md)
    serves the same purpose.

  - A linear or quadratic expression containing decision variables.

  - A nonlinear expression wrapped in
    [`nonlinear()`](https://benet1one.github.io/lpsugar/reference/nonlinear.md).

## Value

The `.problem` with the new `$objective` function.

The `$objective` inherits from
[`ROI::L_objective()`](https://rdrr.io/pkg/ROI/man/L_objective.html),
[`ROI::Q_objective()`](https://rdrr.io/pkg/ROI/man/Q_objective.html), or
[`ROI::F_objective()`](https://rdrr.io/pkg/ROI/man/F_objective.html).

- A quadratic objective function is represented as

  \\\frac{1}{2} x'Qx + Lx\\

- While a nonlinear objective function is simply represented as

  \\F(x)\\

## Details

If `objective` evaluates to a multivariate variable instead of a scalar,
it will apply `sum(objective)` and display a message. Suppress this
message by writing the `sum` yourself.

## See also

[`nonlinear()`](https://benet1one.github.io/lpsugar/reference/nonlinear.md)
For general nonlinear optimization.

## Examples

``` r
# Linear Objective using an Alias ---------------
profit   <- c(Phone = 60, Tablet = 20, eBook = 10)
max_made <- c(Phone = 500, Tablet = 300, eBook = 950)
product  <- names(profit)
fix_cost <- 11e3

p <- lp_problem() |>
    lp_variable(made[product], lower = 0, upper = max_made, integer = TRUE) |>
    lp_alias(total_profit = sum(made * profit)) |>
    lp_maximize(total_profit - fix_cost) |>
    lp_constraint(sum(made) <= 1500)

p$objective
#> linear function:
#> total_profit - fix_cost
#> 

library(ROI.plugin.highs)
s <- lp_solve(p)
print(s)
#> – $variables
#> $made
#> product
#>  Phone Tablet  eBook 
#>    500    300    700 
#> 
#> – $aliases
#> $total_profit
#> [1] 43000
#> 
#> – $objective
#> [1] 32000
#> 
#> – $status
#> Optimal Solution Found ✔ 
#> 

s$aliases$total_profit
#> [1] 43000
sum(c(p$objective$L) * s$variables_vec)
#> [1] 43000

s$aliases$total_profit - fix_cost
#> [1] 32000
s$objective
#> [1] 32000


# Nonlinear objective ---------------------------
nlp <- lp_problem() |> 
    lp_variable(x, lower = 0) |> 
    lp_variable(y, lower = 0) |> 
    lp_maximize(nonlinear(sqrt(x) * log(y))) |> 
    lp_constraint(x + y <= 10)

# There are some different solvers within `nloptr`
library(ROI.plugin.nloptr)
lpsugar_applicable_solvers(nlp)
#> [1] "nloptr.cobyla" "nloptr.mma"    "nloptr.auglag" "nloptr.isres" 
#> [5] "nloptr.slsqp" 

lp_solve(
    nlp, 
    solver = "nloptr.cobyla", 
    start = list(x = 1, y = 1)
)
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
