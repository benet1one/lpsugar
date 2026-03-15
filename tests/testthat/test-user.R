
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
        1 |> parameter(a, b, 1:2),
        "Only vectors and matrices are supported"
    )

    true_mat <- matrix(cm, 2, 3, byrow = TRUE)
    true_mat |> parameter(a, b)

    expect_warning(
        true_mat |> parameter(a, b, byrow = FALSE),
        "Ignoring argument `byrow`"
    )

    expect_error(
        true_mat |> parameter(b, a),
        "not equal to array extent"
    )
})

