
p <- lp_problem() |>
    lp_variable(x[1:3, 1:2]) |>
    lp_variable(y[1:2, 1:3]) |>
    lp_constraint(
        x[1] == 0,
        my_con = x < t(y),
        my_fs = for (i in 1:3) {
            x[i, ] <= y[, i]
        },
        one_line_fs = for (i in 1:3) x[i, ] >= y[, i] - 10
    )

p
p$constraints

x <- p$variables$x
y <- p$variables$y
z <- p$variables$z

x >= t(y) + 1

rbind(x[1, ] == 2, y + x >= 0)


p |> lp_variable(z[1:2]) |> _$constraints

p$constraints[1:3]
p$constraints[1:3, ]
p$constraints["my_con"][2]
p$constraints["my_con", ][2, ]
p$constraints[c("my_con", "one_line_fs")]

test_that("wrong_index", {
    expect_error(p$constraints[])
    expect_error(p$constraints[, 1])
    expect_error(p$constraints[1, 1])
    expect_error(p$constraints[1, , ])
})
