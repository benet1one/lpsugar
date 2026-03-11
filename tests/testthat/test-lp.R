
p <- lp_problem() |>
    lp_variable(x) |>
    lp_variable(y) |>
    lp_alias(x_plus_y = x + y) |>
    lp_maximize(2*x_plus_y) |>
    lp_constraint(
        x + 2*y <= 10,
        2*x + y <= 10
    )

p_updated <- p |>
    lp_variable(z[1:3]) |>
    lp_alias(big_sum = sum(x_plus_y, z)) |>
    lp_maximize(big_sum) |>
    lp_constraint(z < x + y)
