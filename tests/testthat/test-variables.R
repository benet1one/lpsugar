
a <- letters[1:3]

parse_variable_definition(x)
parse_variable_definition((x))
parse_variable_definition({x})
parse_variable_definition(y[a, 1:5])
parse_variable_definition(t[a, b = 1:5])


testthat::expect_error(
    parse_variable_definition(x[, 1:3])
)
testthat::expect_error(
    parse_variable_definition(x[b])
)
testthat::expect_error(
    parse_variable_definition(mean(1))
)
