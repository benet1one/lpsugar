
test_that("objective", {
    a <- letters[1:3]

    p <- lp_problem() |>
        lp_variable(x, integer = TRUE) |>
        lp_variable(y[a], binary = TRUE) |>
        lp_variable(z[1:2, a], lower = 0)

    expect_snapshot(lp_maximize(p, -x) $ objective)
    expect_snapshot(lp_minimize(p, sum(y)) $ objective)

    expect_message(lp_minimize(p, y), "sum\\(y\\)")
    expect_equal(
        lp_minimize(p, y) $ objective $ coef,
        lp_minimize(p, sum(y)) $ objective $ coef
    )

    expect_message(
        p |> lp_minimize({
            k <- 2
            k*y
        }),
        "sum"
    )

    expect_snapshot(
        p |>
            lp_minimize({
                i <- 1
                j <- a[2]
                z[i, j]
            }) |>
            _$objective |>
            unclass()
    )
})

test_that("quadratic objective", {
    p <- lp_problem() |>
        lp_variable(x[1:2]) |>
        lp_variable(y[1:2]) |>
        lp_minimize(sum(x^2) + sum(y))

    expect_snapshot(p$objective)
    expect_snapshot(unclass(p$objective))
})
