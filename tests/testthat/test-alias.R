

test_that("alias", {
    p <- lp_problem() |>
        lp_var(x[1:2, 1:3])

    p |> lp_alias(
        sum_x = sum(x),
        # colsums_x[j = 1:3] <- sum(x[, j])
    )

    expect_error(
        lp_alias(p, sum(x)),
        "must be named"
    )
})
