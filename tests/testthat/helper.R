
problem_variables <- function() {
    a <- letters[1:3]
    p <- lp_problem() |>
        lp_variable(x, integer = TRUE) |>
        lp_variable(y[a], binary = TRUE) |>
        lp_variable(z[1:2, a], lower = 0)
}

problem_constraints <- function() {
    lp_problem() |>
        lp_variable(x[1:3, 1:2]) |>
        lp_variable(y[1:2, 1:3]) |>
        lp_constraint(
            x[1] == 0,
            my_con = x < t(y),
            one_line_fs = for (i in 1:3) x[i, ] >= y[, i] - 10,
            my_fs = for (i in 1:3) {
                x[i, ] <= y[, i]
            },
        )
}
