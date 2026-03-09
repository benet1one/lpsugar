

p <- lp_problem() |>
    lp_variable(x[1:2, 1:2]) |>
    lp_alias(
        sum_x = sum(x),
        # for (i in 1:2) rowsum_x[i] = sum(x[i, ]),

        # Alternatives?
        # rowsum_x[i = 1:2] = sum(x[i, ]),
        # rowsum_x = for (i in 1:2) sum(x[i, ])
    )

p$aliases

test_that("alias_named", {
    expect_error(lp_alias(p, sum(x)))
    expect_error(lp_alias(p, rowsum_x = for (i in 1:2) rowsum_x[i] = sum(x[i, ])))
})
