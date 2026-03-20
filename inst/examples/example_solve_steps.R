p <- lp_problem() |>
    lp_var(x[a = 1:2, b = 1:2], upper = 10) |>
    lp_max(sum(x) + 5)

# Solve directly with lp_solve()
direct_solution <- lp_solve(p)
direct_solution

# Or do each step
model <- make_model(p)
solution_raw <- solve_model(model)
solution_pretty <- pretty_solution(problem = p, solution = solution_raw)

# pretty_solution() gives each variable its dim and dimnames.
solution_raw$variables
solution_pretty$variables

# Notice that in solution_raw, the objective value is 40 instead of 45.
# The objective function is (sum(x) + 5), but the addend (+5) is irrelevant
# for finding the optimum, so lpSolveAPI ignores it.
solution_raw$objective
solution_pretty$objective
