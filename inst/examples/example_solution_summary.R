problem <- lp_problem() |>
    lp_var(x, lower = 0) |>
    lp_var(y[1:3], integer = TRUE) |>
    lp_alias(sum_y = sum(y)) |>
    lp_min(x + y[1]) |>
    lp_con(
        c1 = 2*x - y[3] == 10,
        c2 = for (i in 1:3) y[i] <= i
    )

unfeasible_solution <- list(
    x = -1,
    y = c(4, 2, -3)
)

solution_summary(problem, unfeasible_solution)
