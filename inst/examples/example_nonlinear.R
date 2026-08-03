# max  sqrt(x) * log(y)
#  st  x^y <= 10

p <- lp_problem() |> 
    lp_variable(x, lower = 0) |> 
    lp_variable(y, lower = 1) |> 
    lp_maximize(nonlinear(sqrt(x) * log(y))) |> 
    lp_constraint(nonlinear(x^y) <= 5)

print(p)

library(ROI.plugin.nloptr)
s <- lp_solve(p, solver = "nloptr.cobyla", start = list(x = 2, y = 2))
print(s)
