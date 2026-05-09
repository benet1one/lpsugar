
test_that("quadratic simple", {
    p <- lp_problem() |>
        lp_var(x) |>
        lp_var(y)

    x <- p$variables$x
    y <- p$variables$y

    x*y

    expect_equal(
        (2*x) * (3*y),
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

    expect_equal(
        x*x,
        x^2
    )

    expect_error(
        x^3,
        "Exponent must be 0, 1 or 2"
    )
    expect_error(
        x^2 * y,
        "Non-quadratic operation"
    )
})

test_that("quadratic arrays", {
    p <- lp_problem() |>
        lp_var(z[1:3])

    z <- p$variables$z

    expect_equal(
        z + z^2,
        z^2 + z
    )

    zr <- z * rev(z)
    rz <- rev(z) * z
    zr$ind <- NULL
    rz$ind <- NULL

    expect_equal(zr, rz)

    expect_equal(
        sum(z^2),
        z[1]^2 + z[2]^2 + z[3]^2
    )

    expect_snapshot(z^c(0, 1, 2))
})

test_that("quadratic solver", {
    withr::local_package("ROI.plugin.highs")

    p <- lp_problem() |>
        lp_var(x, lower = 0) |>
        lp_var(y, lower = 0) |>
        lp_min(
            (x - 3)^2 + (x + y - 5)^2
        )


    s <- lp_solve(p)

    expect_equal(
        s$variables,
        list(x = 3, y = 2),
        tolerance = 0.0001
    )

    expect_equal(
        compute_objective(p, list(x = 3, y = 2)),
        s$objective
    )
})
