library(ROI.plugin.highs)

problem <- lp_problem() |>
    lp_var(x[a = 1:2, b = 1:2], upper = 10) |>
    lp_alias(sum_x = sum(x)) |>
    lp_max(sum_x + 5)

# Solve directly with lp_solve()
direct_solution <- lp_solve(problem)
direct_solution

# Or run each step manually
op <- as.OP(problem)
solution_raw <- ROI_solve(op)
solution_pretty <- pretty_solution(problem, solution = solution_raw)

# pretty_solution() gives each variable its dim and dimnames.
solution_raw$solution
solution_pretty$variables

# It also computes aliases
solution_pretty$aliases

# Notice that in solution_raw, the objective value is 40 instead of 45.
# The objective function is (sum(x) + 5), but the addend (+5) is irrelevant
# for finding the optimum, so ROI ignores it.
solution_raw$objval
solution_pretty$objective
