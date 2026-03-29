
test_that("parameter", {
    a <- 1:2
    b <- letters[1:3]

    expect_equal(
        c(2,5) |> parameter(a),
        c(2,5) |> parameter(a = 1:2)
    )

    expect_equal(
        c(6,2,1) |> parameter(b),
        c(6,2,1) |> parameter(b = letters[1:3])
    )

    expect_warning(
        c(2,5) |> parameter(a = 1:2, byrow = FALSE),
        "Ignoring argument `byrow`"
    )

    expect_error(c(3,1) |> parameter(), "cannot be empty")
    expect_error(c(3,1) |> parameter(a,), "Argument 2 can't be empty")
    expect_error(c(3,1) |> parameter(b), "`.x` is length \\(2\\) and `b` is length \\(3\\).")

    cm <- c(
        1, 2, 3,
        4, 5, 6
    )

    cm |> parameter(a, b)
    cm |> parameter(a, b, byrow = FALSE)

    expect_error(
        c(1, 2, 3, 4, 5) |> parameter(a, b),
        r"(`.x` is length \(5\) when it should be length \(6 = 2 x 3\))"
    )

    true_mat <- matrix(cm, 2, 3, byrow = TRUE)
    true_mat |> parameter(a, b)

    expect_warning(
        true_mat |> parameter(a, b, byrow = FALSE),
        "Ignoring argument `byrow`"
    )

    expect_error(
        true_mat |> parameter(b, a),
        r"(`.x` has dimensions \(2 x 3\), while `...` have dimensions \(3 x 2\))"
    )

    arr <- array(dim = c(2, 3, 1))

    expect_error(
        arr |> parameter(a, b, 1),
        "Only vectors and matrices are supported"
    )
    expect_error(
        mat |> parameter(a, b, a),
        "Only vectors and matrices are supported"
    )
})

test_that("solution summary", {
    withr::local_package("ROI")
    p <- lp_problem() |>
        lp_var(y, integer = TRUE) |>
        lp_var(x[1:2, 1:3], lower = 1) |>
        lp_alias(x1 = x[1]) |>
        lp_max(sum(x) + y) |>
        lp_con(
            c1 = for (i in seq_along(x)) x[i] < 10,
            c2 = 2*x/6 + y < 50,
            x[1] + x[2] == 4
        )

    s <- lp_solve(p)
    s

    slist <- s$variables
    slist$y <- NULL
    slist$x[1, 1] <- 50
    slist$x[2, 2] <- NA
    slist$x[2, 3] <- -5

    expected <- s$variables_vec
    expected["y"] <- 0
    expected["x[1,1]"] <- 50
    expected["x[2,2]"] <- 1
    expected["x[2,3]"] <- -5

    expect_equal(
        solution_to_vec(p, s),
        s$variables_vec
    )
    expect_equal(
        solution_to_vec(p, slist),
        expected
    )

    slist2 <- slist
    slist2$y <- 2.5

    expect_warning(
        solution_to_vec(p, slist2),
        "'y' should be integer"
    )

    slist3 <- slist
    slist3$x <- slist$x[1:2, 1:2]

    expect_error(
        solution_to_vec(p, slist3),
        r"(Variable 'x' in `solution` is length \(4\) when it should be length \(6\))"
    )

    svec <- s$variables_vec |> head(4)
    expect_error(
        solution_to_vec(p, svec),
        r"(`problem` has \(7\) variables but `solution` is length \(4\))"
    )

    expect_snapshot(solution_summary(p, slist))
})

