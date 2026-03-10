
a <- letters[1:3]

p <- lp_problem() |>
    lp_variable(x, integer = TRUE) |>
    lp_variable(y[a], binary = TRUE) |>
    lp_variable(z[1:2, a], lower = 0)

print(p)
x <- p$variables$x
y <- p$variables$y
z <- p$variables$z

y[1:2]
y[-3]
y[c(TRUE, TRUE, FALSE)]

y["b"]
y[c("a", "b")]

rev(y)
t(z)
z[2:1, ]

z[1, ] $ ind
z[1, , drop = TRUE] $ ind


x2 <- x + y
x3 <- x2 + y

x4 <- add_v_c(x3, 2)
x5 <- add_v_c(x3, 2:4)

x6 <- multiply_v_c(x5, 3)
x7 <- multiply_v_c(x5, 3:1)


test_that("variable definitions", {
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
})



test_that("variable indexing", {
    expect_error(y[4], "out of bounds")
    expect_error(y[-4], "out of bounds")
    expect_error(y[0], "invalid subscript")
    expect_error(y["d"], "invalid subscript 'd'")
    expect_identical(rev(y), y[rev(a)])
    expect_identical(z$coef, t(t(z))$coef)
    expect_snapshot(t(z))
})

test_that("operations", {
    expect_identical(x, +x)
    expect_identical(x/2, x*0.5)
    expect_identical(1-y, !y)
    expect_identical(2*y, y + y)
    expect_identical(-z, z - 2*z)
    expect_error(x*y, "cannot multiply two variables")
    expect_error(x^2, "cannot use powers or exponentials")
    expect_error(2/x, "cannot divide by a variable")
    expect_error(!x, "only supported for binary variables")
})

test_that("sum", {
    expect_identical(
        sum(x, y, 2*z, 1:4),
        x + y[1] + y[2] + y[3] + 2*sum(z) + sum(1:4)
    )
    expect_identical(
        sum(z[1:5]) $ coef,
        cumsum(z)[5] $ coef
    )
})
