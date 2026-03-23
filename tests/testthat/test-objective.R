test_that("objective", {
    a <- letters[1:3]

    p <- lp_problem() |>
        lp_variable(x, integer = TRUE) |>
        lp_variable(y[a], binary = TRUE) |>
        lp_variable(z[1:2, a], lower = 0)

    expect_snapshot(lp_maximize(p, -x) $ objective)
    expect_snapshot(lp_minimize(p, sum(y)) $ objective)

    expect_message(lp_minimize(p, y), "sum\\(y\\)")
    expect_message(
        p |> lp_minimize({
            k <- 2
            k*y
        }),
        "sum"
    )

    expect_snapshot(
        p |> lp_minimize({
            i <- 1:2
            j <- 1:3
            z[i, j] * outer(i, j, `^`)
        }) |> _$objective
    )
})
