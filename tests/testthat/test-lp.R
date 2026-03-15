
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


test_that("printing", {
    lp_problem() |>
        lp_variable(x, lower = 0) |>
        lp_variable(y, lower = 0, integer = TRUE) |>
        lp_maximize(2*x_plus_y) |>
        lp_constraint(
            x + 2*y <= 10,
            2*x + y <= 10
        ) |>
        print()
})

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

test_that("feasible", {
    no_obj <- lp_problem() |>
        lp_variable(x, lower = 5, upper = 10)

    expect_error(
        lp_solve(no_obj),
        "Did you forget to set the objective function"
    )

    s <- no_obj |> lp_minimize(0) |> lp_solve()
    f <- no_obj |> lp_find_feasible()

    s$pointer <- NULL
    f$pointer <- NULL

    expect_equal(s, f)
})
