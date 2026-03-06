
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

p$variables$y[1:2]
p$variables$y[-3]
p$variables$y[c(TRUE, TRUE, FALSE)]

p$variables$y["b"]
p$variables$y[c("a", "b")]


testthat::test_that("variable indexing", {
    testthat::expect_error(p$variables$y[4])
    testthat::expect_error(p$variables$y[0])
    testthat::expect_error(p$variables$y["d"])
})
