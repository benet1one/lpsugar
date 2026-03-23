
test_that("alias", {
    withr::local_package("ROI")

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
})
