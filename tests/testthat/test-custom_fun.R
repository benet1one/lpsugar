

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
    expect_true({
        w_mean_x <- p |> lp_eval(weighted.mean(x, w = c(0, 5)))
        all(w_mean_x$coef == c(0, 1, 0))
    })
    expect_no_error(
        p |> lp_eval(weighted.mean(1:2, 2:3))
    )
    expect_warning(
        p |> lp_eval(weighted.mean(x, 1:2, na.rm = TRUE)),
        "Ignoring argument `na.rm`"
    )
    expect_error(
        p |> lp_eval(weighted.mean(x, 1:2, hi = "hi")),
        "must be empty" # dots
    )
    expect_error(
        p |> lp_eval(weighted.mean(1:2, w = x)),
        "Weights `w` cannot be a variable"
    )
})

test_that("sum over", {
    a <- matrix(1:6, nrow = 3, ncol = 2)
    p <- lp_problem() |>
        lp_var(x[1:3])

    s1 <- p |> lp_eval(sum_over(j = 1:2, x * a[, j]))
    s2 <- p |> lp_eval(sum_over(i = 1:3, j = 1:2, x[i] * a[i, j]))

    expect_equal(s1$coef, c(5, 7, 9), ignore_attr = TRUE)
    expect_equal(s2$coef, c(5, 7, 9), ignore_attr = TRUE)

    q <- lp_problem() |>
        lp_var(y[1:3, 1:3])

    s3 <-  q |> lp_eval(sum_over(i = 1:3, diag(y)[i] * i))

    expect_equal(
        s3$coef,
        c(1, 0, 0, 0, 2, 0, 0, 0, 3),
        ignore_attr = TRUE
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

test_that("ifelse1", {
    withr::local_package("ROI")

    n <- 10
    upper <- 50

    p <- lp_problem() |>
        lp_var(y[1:n], upper = upper) |>
        lp_var(less_than_index[1:n], binary = TRUE) |>
        lp_max(sum(y)) |>
        lp_con(sum(less_than_index) >= 5)

    p1 <- p |> lp_con(
        y <= ifelse(less_than_index, yes = 1:n, no = upper)
    )
    p2 <- p |> lp_con(
        y <= ifelse(!less_than_index, yes = upper, no = 1:n)
    )
    p3 <- p |> lp_con(
        for (i in 1:n) {
            y[i] <= ifelse(less_than_index[i], yes = i, no = upper)
        }
    )

    expect_equal(p1$constraints$lhs, p2$constraints$lhs)
    expect_equal(p1$constraints$rhs, p2$constraints$rhs)

    expect_equal(
        as.matrix(p1$constraints$lhs),
        as.matrix(p3$constraints$lhs),
        ignore_attr = TRUE
    )
    expect_equal(p1$constraints$rhs, p3$constraints$rhs)

    s <- lp_solve(p1)
    s

    expect_error(
        p |> lp_con(ifelse(less_than_index, y <= 1:n, y <= upper)),
        "`yes` and `no` cannot be constraints"
    )
})

test_that("ifelse2", {
    n <- 6
    x_coef <- c(0.2, 1.4, 1.1, 0.8, 0.5, 2.6)

    p <- lp_problem() |>
        lp_var(x[1:n], lower = 0) |>
        lp_var(cap[1:n], upper = 10) |>
        lp_var(cap_special, upper = 10) |>
        lp_con(
            x < ifelse(1:n <= 2, cap_special, cap),
            x > ifelse(1:n <= 3, 1:n, cap_special/10),
            x == ifelse(1:n <= 4, 2, 8)
        )

    withr::local_options(width = 100)
    expect_snapshot(p$constraints)
})
