library(ROI.plugin.alabama)

## Simple Example -----------------------
# max  sqrt(x) * log(y)
#  st  x + y <= 10

p1 <- lp_problem() |> 
    lp_variable(x, lower = 0) |> 
    lp_variable(y, lower = 0) |> 
    lp_maximize_function(\(x, y) sqrt(x) * log(y)) |> 
    lp_constraint(x + y <= 10)

lp_solve(p1, start = list(x = 1, y = 1))
