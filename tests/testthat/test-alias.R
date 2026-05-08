
test_that("alias", {
    withr::local_package("ROI.plugin.highs")

    p <- lp_problem() |>
        lp_var(x[1:2, 1:3], lower = matrix(1:6, nrow = 2))

    p |> lp_alias(
        sum_x = sum(x),
        # colsums_x[j = 1:3] <- sum(x[, j])
    )

    dx <- p |>
        lp_alias(dx = diag(x)) |>
        lp_min(sum(x)) |>
        lp_solve() |>
        _$aliases$dx

    expect_equal(c(dx), c(1, 4))

    expect_error(
        lp_alias(p, sum(x)),
        "must be named"
    )

    expect_message(
        p2 <- p |>
            lp_alias(s = x[1]) |>
            lp_alias(s = x[2]),
        "Overriding alias `s`"
    )
    expect_true(
        all(p2$aliases$s$coef == c(0, "x[2,1]" = 1, 0, 0, 0, 0))
    )

    expect_error(
        p |> lp_alias(x = x[1]),
        "Cannot override variable `x`"
    )
    expect_error(
        p |> lp_alias(y = 1:3),
        "Did not evaluate to a variable"
    )
})
