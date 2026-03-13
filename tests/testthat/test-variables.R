
test_that("variable definitions", {
    a <- letters[1:3]

    expect_snapshot(
        parse_variable_definition(t[a, b = 1:5])
    )
    expect_snapshot(
        parse_variable_definition(t[b = a, 1:5])
    )
})

test_that("variable definition errors", {
    expect_error(
        lp_problem() |> lp_variable(x[b])
    )
    expect_error(
        lp_problem() |> lp_variable()
    )
    expect_error(
        lp_problem() |> lp_variable(x[, 1:3]),
        "cannot be missing."
    )
    expect_error(
        lp_problem() |> lp_variable(x[1:3, ]),
        "cannot be missing."
    )
    expect_error(
        lp_problem() |> lp_variable(mean(1)),
        "Failed to parse variable"
    )
    expect_error(
        lp_problem() |> lp_variable(mean()[1]),
        "Failed to parse variable"
    )

    expect_error(
        lp_problem() |> lp_variable(x) |> lp_variable(x[1:5]),
        "already exists"
    )
})

test_that("variable bounds", {
    expect_error(
        lp_problem() |> lp_variable(x, lower = 2, upper = 1),
        "cannot be greater than `upper` bound"
    )
    expect_error(
        lp_problem() |> lp_variable(x, lower = +Inf),
        "cannot be \\+Inf"
    )
    expect_error(
        lp_problem() |> lp_variable(x, upper = -Inf),
        "cannot be \\-Inf"
    )

    expect_warning(
        p1 <- lp_problem() |> lp_variable(x, lower = NULL),
        "`NULL` or zero-length, setting to -Inf"
    )
    expect_equal(p1$variables$x$lower, -Inf)


    expect_warning(
        p2 <- lp_problem() |> lp_variable(x[1:3], upper = c(2, NA, 1)),
        "containts NA values, setting to Inf"
    )
    expect_equal(p2$variables$x$upper, c(2, +Inf, 1))

    expect_error(
        lp_problem() |> lp_variable(x, upper = "3"),
        "not numeric"
    )

    expect_error(
        lp_problem() |> lp_variable(x[1:2, 1:3], lower = matrix(0, nrow = 3, ncol = 2)),
        "`dim\\(lower\\)` different"
    )

    # expect_warning(
    #     lp_problem() |> lp_variable(x, binary = TRUE, lower = 0, upper = 1),
    #     "Ignoring bounds"
    # )
})


test_that("variable indexing", {
    a <- letters[1:3]
    p <- problem_variables()
    x <- p$variables$x
    y <- p$variables$y
    z <- p$variables$z

    expect_identical(y[1:2], y[-3])
    expect_identical(y[1:2], y[c(TRUE, TRUE, FALSE)])
    expect_identical(y[1:2], y[c("a", "b")])
    expect_identical(rev(y), y[3:1])
    expect_identical(rev(y), y[rev(a)])

    z1 <- z[1, ] $ ind
    z1_dropped <- z[1, , drop = TRUE] $ ind

    expect_all_true(dim(z1) == c(1, 3))
    expect_true(is.null(dim(z1_dropped)))

    expect_error(y[4], "out of bounds")
    expect_error(y[-4], "out of bounds")
    expect_error(y[0], "Invalid subscript")
    expect_error(y["d"], "Invalid subscript 'd'")

    # Transpose
    expect_identical(z$coef, t(t(z))$coef)
    expect_snapshot(t(z))
    expect_error(t(y), "two-dimensional")

    p2 <- lp_problem() |> lp_variable(ThreeD[1:2, 1:3, 1:3])
    ThreeD <- p2$variables$ThreeD

    expect_error(
        t(ThreeD[, , 1]),
        "Index it with"
    )
    expect_snapshot(
        t(ThreeD[, , 1, drop = TRUE])
    )
})

test_that("operations", {
    p <- problem_variables()
    x <- p$variables$x
    y <- p$variables$y
    z <- p$variables$z

    expect_identical(x, +x)
    expect_identical(x/2, x*0.5)
    expect_identical(1-y, !y)
    expect_identical(2*y, y + y)
    expect_identical(-z, z - 2*z)

    expect_error(c(1, 2, NA) + y, "Left-hand-side object contains NA values")
    expect_error(y / c(1, 2, NA), "Right-hand-side object contains NA values")

    expect_error(y + z, "Non-conformable")
    expect_error(y * 1:2, "Non-conformable")
    expect_error(x*y, "Cannot multiply two variables")
    expect_error(x^2, "Cannot use powers or exponentials")
    expect_error(2/x, "Cannot divide by a variable")
    expect_error(!x, "only supported for binary variables")

    expect_error(abs(x), "absolute value")
    expect_error(exp(x), "exp")

    expect_error(y | y, "Unsupported operation")
    expect_error(y & y, "Unsupported operation")
    expect_error(y %% y, "Unsupported operation")
    expect_error(y %/% y, "Unsupported operation")
    expect_error(
        x & sum({
            k <- 2
            k*y
        }),
        "Unsupported operation"
    )
})

test_that("sum", {
    p <- problem_variables()
    x <- p$variables$x
    y <- p$variables$y
    z <- p$variables$z

    expect_identical(
        sum(x, y, 2*z, 1:4),
        x + y[1] + y[2] + y[3] + 2*sum(z) + sum(1:4)
    )
    expect_identical(
        sum(z[1:5]) $ coef,
        cumsum(z)[5] $ coef
    )

    expect_no_error(
        sum(y, c(1, 1, NA), na.rm = TRUE)
    )
    expect_error(
        sum(y, c(1, 1, NA), na.rm = FALSE) # Default
    )
})
