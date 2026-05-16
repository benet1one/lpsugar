# Set an objective function

Minimize of maximize an expression.

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

  Expression to optimize, which must evaluate to an `lp_variable`
  object. Alternatively, set `objective = 0` to make the solver find a
  feasible solution instead of optimizing, just like
  [`lp_find_feasible()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md)
  does.

## Value

The `.problem` with the updated `$objective` function, a list with these
fields:

- `$coef` : Vector with the coefficients for each variable.

- `$add` : Numeric, addend to the final value. It is not used in the
  solver.

- `$direction` : String, goal of the solver. Can be `"minimize"` or
  `"maximize"`.

- `$expr` : String, expression that defined the objective function.

## Details

If `objective` evaluates to a multivariate variable instead of a scalar,
it will apply `sum(objective)` and display a message. Suppress this
message by writing the `sum` yourself.

## Examples

``` r
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
#> maximize total_profit - fix_cost 
#> sum(coef*vars) - 11000
#> $coef
#>  made[Phone] made[Tablet]  made[eBook] 
#>           60           20           10 
#> 

library(ROI) |> suppressMessages()
(s <- lp_solve(p))
#> $variables
#> $variables$made
#> product
#>  Phone Tablet  eBook 
#>    500    300    700 
#> 
#> 
#> $aliases
#> $aliases$total_profit
#> [1] 43000
#> 
#> 
#> $objective
#> [1] 32000
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

s$aliases$total_profit
#> [1] 43000
sum(p$objective$coef * s$variables_vec)
#> [1] 43000

s$objective
#> [1] 32000
sum(p$objective$coef * s$variables_vec) + p$objective$add
#> [1] 32000
```
