

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
