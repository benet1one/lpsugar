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

library(ROI.plugin.highs)
s <- lp_solve(p)
print(s)

s$aliases$total_profit
sum(c(p$objective$L) * s$variables_vec)

s$aliases$total_profit - fix_cost
s$objective


# Nonlinear objective ---------------------------

nl <- lp_problem() |> 
    lp_variable(x, lower = 0) |> 
    lp_variable(y, lower = 0) |> 
    lp_maximize(nonlinear(sqrt(x) * log(y))) |> 
    lp_constraint(x + y <= 10)

# There are some different solvers within `nloptr`
library(ROI.plugin.nloptr)
lpsugar_applicable_solvers(nl)

lp_solve(
    nl, 
    solver = "nloptr.cobyla", 
    start = list(x = 1, y = 1)
)

# See more examples in the Nonlinear vignette
# vignette("nonlinear", package = "lpsugar")
