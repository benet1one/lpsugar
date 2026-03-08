
a <- letters[1:3]

p <- lp_problem() |>
    lp_variable(x, integer = TRUE) |>
    lp_variable(y[a], binary = TRUE) |>
    lp_variable(z[1:2, a], lower = 0)

p |> lp_minimize(-x)
p |> lp_minimize(y)
p |> lp_minimize(sum(y))


testthat::test_that("objective", {
    testthat::expect_message(lp_minimize(p, y))
})
