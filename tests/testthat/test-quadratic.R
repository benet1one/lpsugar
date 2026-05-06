
test_that("quadratic simple", {
    p <- lp_problem() |>
        lp_var(x) |>
        lp_var(y)

    x <- p$variables$x
    y <- p$variables$y

    x*y

    expect_equal(
        2*x * 3*y,
        6 * (x*y)
    )

    expect_equal(
        x * (y + 2),
        x*y + x*2
    )

    expect_equal(
        (x + 2) * (y + 3),
        x*y + 2*y + x*3 + 2*3
    )

    x^2
    (x + 3)^2
    (x + y)^2
})

test_that("quadratic arrays", {
    p <- lp_problem() |>
        lp_var(z[1:3])

    z <- p$variables$z

    (z * z[1] * 1:3) + z
    z^c(0, 1, 2)
})
