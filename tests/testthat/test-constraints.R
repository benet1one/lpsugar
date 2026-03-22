
test_that("printing", {
    plong <- lp_problem() |>
        lp_var(x) |>
        lp_con(x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x > 1)

    print(plong$constraints)
    print(plong$constraints, compact = TRUE) |> expect_snapshot()

    p <- problem_constraints()
    print(p$constraints)
    print(p$constraints, compact = TRUE) |> expect_snapshot()
})

test_that("constraint updates", {
    p <- problem_constraints()
    p |> lp_variable(z[1:2]) |> _$constraints
    p |> lp_variable(z[1:2]) |> lp_constraint(x[2] <= 4*z[2]) |> _$constraints
})

test_that("non constraint", {
    p <- problem_constraints()

    expect_error(
        p |> lp_constraint(1 <= 2),
        "does not contain any variables"
    )
    expect_error(
        p |> lp_constraint(2*x),
        "did not evaluate to a constraint"
    )
    expect_error(
        p |> lp_constraint(y[1] != 0),
        "Inequality"
    )
})

test_that("indexing constraints", {
    p <- problem_constraints()

    p$constraints[1:3]
    p$constraints[1:3, ]
    p$constraints["my_con"][2]
    p$constraints["my_con", ][2, ]
    p$constraints[c("my_con", "one_line_fs")]

    expect_error(p$constraints[], "Index constraints with")
    expect_error(p$constraints[, 1], "Index constraints with")
    expect_error(p$constraints[1, 1], "Index constraints with")
    expect_error(p$constraints[1, , ], "Index constraints with")
})

test_that("rbind constraints", {
    p <- problem_constraints()
    x <- p$variables$x
    y <- p$variables$y

    expect_no_error(rbind(x == 1, y >= 0))
    expect_error(rbind(x == 1, y), "with other classes")
    expect_error(rbind(0, x == 1), "with other classes")
})

test_that("unique constraint names", {
    p <- lp_problem() |>
        lp_var(x[1:3]) |>
        lp_subject_to(
            first = x > 0,
            x[1] == 0,
            x[2] == 1,
            fourth = x[3] < 6,
            fifth = x < 8,
            x == 5
        )

    s <- lp_find_feasible(p)
    expect_snapshot(s$pointer)
})
