
a <- letters[1:3]

p <- lp_problem() |>
    lp_variable(x, integer = TRUE) |>
    lp_variable(y[a], binary = TRUE) |>
    lp_variable(z[1:2, a], lower = 0)

test_that("objective snaps", {
    expect_snapshot(lp_maximize(p, -x) $ objective)
    expect_snapshot(lp_minimize(p, sum(y)) $ objective)
})

test_that("sum message", {
    expect_message(lp_minimize(p, y), "sum")
})
