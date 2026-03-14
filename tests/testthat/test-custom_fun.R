

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

    expect_equal(
        p |> lp_eval(sum(x, NA, na.rm = TRUE)),
        p |> lp_eval(sum(x))
    )
    expect_true({
        x2 <- p |> lp_eval(sum(c(1, NA, 1), x, na.rm = TRUE))
        x2$add == 2
    })

    expect_true({
        mean_x <- p |> lp_eval(mean(x))
        all(mean_x$coef == c(1/2, 1/2, 0))
    })
    expect_true({
        w_mean_x <- p |> lp_eval(weighted.mean(x, w = c(1, 3)))
        all(w_mean_x$coef == c(1/4, 3/4, 0))
    })
    expect_warning(
        p |> lp_eval(weighted.mean(x, 1:2, na.rm = TRUE)),
        "Ignoring argument `na.rm`"
    )
    expect_error(
        p |> lp_eval(weighted.mean(x, 1:2, hi = "hi")),
        "must be empty" # dots
    )
})

test_that("apply", {
    p <- lp_problem() |>
        lp_var(y[1:2, 1:2]) |>
        lp_var(z[1:2, 1:2, 1:2])

    p |> lp_eval(apply(matrix(1, 2, 3), 1, sum))
    p |> lp_eval(apply(matrix(1, 2, 3), 2, sum))

    p |> lp_eval(apply(y, 1, mean))
    p |> lp_eval(apply(y, 2, mean))

    rowmeans_z <- p |> lp_eval(apply(z, 1, mean))
    rowmeans_z
    rowmeans_z$ind

    expect_all_true(
        dim(rowmeans_z$ind) == c(1, 2)
    )

    rowcolmeans_z <- p |> lp_eval(apply(z, 1:2, mean))
    rowcolmeans_z
    rowcolmeans_z$ind

    expect_all_true(
        dim(rowcolmeans_z$ind) == c(2, 2)
    )

    deepmeans_z <- p |> lp_eval(apply(z, 3, mean))
    deepmeans_z
    deepmeans_z$ind

    expect_equal(
        p |> lp_eval(rowSums(y)),
        p |> lp_eval(apply(y, 1L, sum))
    )
    expect_equal(
        p |> lp_eval(rowSums(matrix(1, 2, 2))),
        rowSums(matrix(1, 2, 2))
    )
    expect_warning(
        p |> lp_eval(rowSums(y, dims = 3L)),
        "Ignoring argument `dims`"
    )
})
