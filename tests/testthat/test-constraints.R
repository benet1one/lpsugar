
p <- lp_problem() |>
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

p
p$constraints

x <- p$variables$x
y <- p$variables$y
z <- p$variables$z

# Updates
p |> lp_variable(z[1:2]) |> _$constraints
p |> lp_variable(z[1:2]) |> lp_constraint(x[2] <= 4*z[2])


test_that("non constraint", {
    expect_error(
        lp_constraint(p, 1 <= 2),
        "does not contain any variables"
    )
    expect_error(
        lp_constraint(p, 2*x),
        "did not evaluate to a constraint"
    )
})

test_that("indexing constraints", {
    p$constraints[1:3]
    p$constraints[1:3, ]
    p$constraints["my_con"][2]
    p$constraints["my_con", ][2, ]
    p$constraints[c("my_con", "one_line_fs")]

    expect_error(p$constraints[], "index constraints with")
    expect_error(p$constraints[, 1], "index constraints with")
    expect_error(p$constraints[1, 1], "index constraints with")
    expect_error(p$constraints[1, , ], "index constraints with")
})

test_that("rbind constraints", {
    expect_no_error(rbind(x == 1, y >= 0))
    expect_error(rbind(x == 1, y), "with other classes")
    expect_error(rbind(0, x == 1), "with other classes")
})
