
p <- lp_problem() |>
    lp_variable(x, lower = 0) |>
    lp_variable(y, lower = 0, integer = TRUE) |>
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


ptr <- make_model(p)
ptr

sol <- solve_model(ptr)
sol

pretty_solution(p, sol)


p_updated |> lp_solve()



test_that("solving with multivariate bounds", {
    s <- lp_problem() |>
        lp_variable(x[1:2, 1:2], lower = matrix(1:4, 2, 2), upper = 10) |>
        lp_minimize(x[1] + x[2] + x[3] - x[4]) |>
        lp_solve()

    expect_equal(
        s$variables$x,
        matrix(c(1, 2, 3, 10), 2, 2),
        ignore_attr = TRUE
    )
})
