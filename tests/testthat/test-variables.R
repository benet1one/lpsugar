
a <- letters[1:3]

parse_variable_definition(x)
parse_variable_definition((x))
parse_variable_definition({x})
parse_variable_definition(y[a, 1:5])
parse_variable_definition(t[a, b = 1:5])


test_that("variable definitions", {
    testthat::expect_error(
        parse_variable_definition(x[, 1:3])
    )
    testthat::expect_error(
        parse_variable_definition(x[b])
    )
    testthat::expect_error(
        parse_variable_definition(mean(1))
    )
})


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


testthat::test_that("variable indexing", {
    testthat::expect_error(y[4])
    testthat::expect_error(y[0])
    testthat::expect_error(y["d"])
    testthat::expect_identical(rev(y), y[rev(a)])
})

testthat::test_that("operations", {
    testthat::expect_identical(x, +x)
    testthat::expect_identical(x/2, x*0.5)
    testthat::expect_identical(1-y, !y)
    testthat::expect_identical(2*y, y + y)
    testthat::expect_identical(-z, z - 2*z)
    testthat::expect_error(x*x)
    testthat::expect_error(x^2)
    testthat::expect_error(2/x)
    testthat::expect_error(!x)
})

testthat::test_that("sum", {
    testthat::expect_identical(
        sum(x, y, 2*z, 1:4),
        x + y[1] + y[2] + y[3] + 2*sum(z) + sum(1:4)
    )
    testthat::expect_identical(
        sum(z[1:5]) $ coef,
        cumsum(z)[5] $ coef
    )
})
