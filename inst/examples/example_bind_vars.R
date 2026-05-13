# y[i] is a variable such that
# y[i] > y[i-1]
# Y0 is a parameter indicating what would be y[0]

Y0 <- 4

p <- lp_problem() |>
    lp_var(y[1:3]) |>
    lp_alias(y_full = bind_vars(Y0, y)) |>
    lp_con(for (i in 2:length(y_full))
        y_full[i] > y_full[i-1]
    )

p$constraints
