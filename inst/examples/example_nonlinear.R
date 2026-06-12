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
lp_solve(p1, solver = "nloptr.cobyla", start = list(x = 1, y = 1))

# See more examples in the Nonlinear vignette
# vignette("nonlinear", package = "lpsugar")
