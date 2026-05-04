
test_that("printing", {
    plong <- lp_problem() |>
        lp_var(x) |>
        lp_con(x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x > 1)

    print(plong$constraints)
    print(plong$constraints, compact = TRUE) |> expect_snapshot()

    p <- problem_constraints()
    print(p$constraints)
    print(p$constraints, compact = TRUE) |> expect_snapshot()

    p_many_rows <- lp_problem() |>
        lp_var(y[1:3]) |>
        lp_con(
            for (i in 1:50) y[i %% 3 + 1] <= i
        )

    expect_snapshot(
        print(p_many_rows, compact = FALSE, max_rows = 5)
    )
})

test_that("constraint updates", {
    p <- lp_problem() |>
        lp_var(x[1:3]) |>
        lp_con(x >= 0)

    p2 <- p |> lp_variable(z[1:2])
    expect_equal(
        dim(p2$constraints$lhs), c(3, 5)
    )

    p3 <- p2 |> lp_constraint(x[1:2] <= 4*z[1:2])
    expect_equal(
        dim(p3$constraints$lhs), c(5, 5)
    )

    p$constraints
    p2$constraints
    p3$constraints
})

test_that("deleting constraints", {
    p <- lp_problem() |>
        lp_variable(x[1:3]) |>
        lp_subject_to(
            first = (x > 0),
            second = for (i in 2:3) x[i] > x[i-1],
            x[1] < 5
        )

    expect_true({
        p2 <- p |> lp_delete_constraint(c("first"))
        all(p2$constraints$name == c("second", "second", ""))
    })
    expect_true({
        p3 <- p |> lp_delete_constraint(c("first", "second"))
        p3$constraints$name == ""
    })

    expect_warning(
        p |> lp_delete_constraint(c("second", "third", "fourth")),
        'The following constraints are not defined.+"third", "fourth"'
    )
    expect_warning(
        p |> lp_delete_constraint(c("second", "<unnamed>")),
        "Cannot delete unnamed constraints"
    )
    expect_warning(
        p |> lp_delete_constraint(c("second", "")),
        "Cannot delete unnamed constraints"
    )
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

    expect_equal(
        p$constraints[1:3],
        p$constraints[1:3, ]
    )
    expect_equal(
        p$constraints["my_con"][2],
        p$constraints["my_con", ][2, ]
    )

    expect_equal(
        p$constraints["my_con"] |> head(2),
        p$constraints["my_con"][1:2]
    )

    p$constraints[c("my_con", "one_line_fs")]

    expect_error(p$constraints[], "Index constraints with")
    expect_error(p$constraints[, 1], "Index constraints with")
    expect_error(p$constraints[1, 1], "Index constraints with")
    expect_error(p$constraints[1, , ], "Index constraints with")
})

test_that("bind constraints", {
    p <- problem_constraints()
    x <- p$variables$x
    y <- p$variables$y

    expect_equal(
        bind_cons(x == 1, y >= 0),
        rbind(x == 1, y >= 0)
    )

    expect_error(
        rbind(x == 1, y),
        "`bind_cons\\(\\)` can only bind `lp_constraint`, not `lp_variable`"
    )
    expect_error(
        rbind(0, x == 1),
        "`bind_cons\\(\\)` can only bind `lp_constraint`, not `numeric`"
    )

    n <- 3
    l <- 0
    u <- 5

    q <- lp_problem() |>
        lp_var(y[1:n], lower = l, upper = u) |>
        lp_var(is_two[1:n], binary = TRUE) |>
        lp_con(
            name_outer = for (i in 1:n) bind_cons(
                y[i] >= l + is_two[i] * (2-l),
                y[i] <= u - is_two[i] * (u-2)
            )
        )

    expect_snapshot(q$constraints)
})

test_that("conditional constraints", {
    cond <- c(FALSE, TRUE, TRUE, FALSE, TRUE)
    n <- length(cond)

    p <- lp_problem() |> lp_var(x[1:n], binary = TRUE)

    p |>
        lp_con(if (FALSE) x[1] == 2) |>
        _$constraints

    p |>
        lp_con(if (FALSE) x[1] == 2) |>
        lp_con(x[2] >= 5) |>
        _$constraints


    expect_snapshot({
        p_if_for <- p

        for (i in 1:n) {
            p_if_for <- p_if_for |> lp_con(
                cc = if (cond[i]) x[i] <= 0
            )
        }

        p_if_for$constraints
    })

    expect_snapshot({
        p_for_if <- p |> lp_con(
            cc = for (i in seq_along(x)) if (cond[i]) {
                x[i] <= 0
            }
        )

        p_for_if$constraints
    })
})

test_that("quadruple for", {
    p <- lp_problem() |>
        lp_var(x[1:100]) |>
        lp_con(for (i in 1:3) for (j in 1:2) for (k in 1:2) for (m in 1:2) {
            x[i+j+k+m] <= i*j - k*m
        })

    expect_snapshot(rownames(p$constraints))
})
