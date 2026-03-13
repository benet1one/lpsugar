

test_that("diag", {
    p <- lp_problem() |>
        lp_var(x[1:2, 1:2]) |>
        lp_var(diag)

    # Variable
    expect_true(p |> lp_eval(diag) |> is_lp_variable())
    p |> lp_eval(2*diag + 1)

    # Identity matrix
    expect_snapshot(p |> lp_eval(diag(1, nrow = 3)))

    # Diag(x)
    expect_snapshot(p |> lp_eval(diag(x)))
    expect_no_error(p |> lp_eval(diag(x) + diag))
    expect_warning(
        p |> lp_eval(diag(x, nrow = 2)),
        "Ignoring argument `nrow`"
    )


    # Misc
    p |> lp_eval(x + diag(1, nrow = 2))
    p |> lp_eval(x + diag(1, nrow = 2) - diag)

    expect_error(
        p |> lp_eval(diag(diag)),
        "not two-dimensional"
    )

})

test_that("sum", {
    p <- lp_problem() |>
        lp_variable(x[1:2]) |>
        lp_variable(y)

    expect_equal(
        p |> lp_eval(sum(1:2, 1)),
        4
    )

    expect_equal(
        p |> lp_eval(sum(1:2, x)),
        p |> lp_eval(sum(x, 1:2))
    )

    expect_equal(
        p |> lp_eval(sum(x, y)),
        p |> lp_eval(sum(x) + y)
    )

    expect_error(
        p |> lp_eval(sum(x, NA)),
        "Right-hand-side object contains NA values"
    )

    expect_no_error(
        p |> lp_eval(sum(x, NA, na.rm = TRUE))
    )
    expect_true({
        x2 <- p |> lp_eval(sum(c(1, NA, 1), x, na.rm = TRUE))
        x2$add == 2
    })
})
