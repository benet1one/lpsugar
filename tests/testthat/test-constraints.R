
p <- lp_problem() |>
    lp_variable(x[1:3, 1:2]) |>
    lp_variable(y[1:2, 1:3]) |>
    lp_constraint(
        x[1] == 0,
        my_con = x < t(y),
        my_fs = for (i in 1:3) {
            x[i, ] <= y[, i]
        },
    )

x <- p$variables$x
y <- p$variables$y
z <- p$variables$z

x >= t(y) + 1

rbind(x[1, ] == 2, y + x >= 0)
