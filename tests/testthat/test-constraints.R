
test_that("printing", {
    p <- problem_constraints()
    
    plong <- lp_problem() |>
        lp_var(x) |>
        lp_con(x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x+x > 1)

    p_many_rows <- lp_problem() |>
        lp_var(y[1:3]) |>
        lp_con(
            for (i in 1:20) y[i %% 3 + 1] <= i
        )
    
    p_many_cols <- lp_problem() |>
        lp_var(z[1:300]) |>
        lp_con(z >= 0, z <= 10)
    
    print(p$constraints, full = FALSE) |> expect_snapshot()
    print(p$constraints) |> expect_snapshot()
    
    print(plong$constraints, full = FALSE) |> expect_snapshot()
    print(plong$constraints) |> expect_snapshot()
    
    print(p_many_rows, full = TRUE) |> expect_snapshot()
    print(p_many_cols, full = TRUE) |> expect_snapshot()
})

test_that("constraint updates", {
    p <- lp_problem() |>
        lp_var(x[1:3]) |>
        lp_con(x^2 >= 0)

    p2 <- p |> lp_variable(z[1:2])

    expect_equal(
        dim(p2$constraints$L),
        c(3, 5)
    )
    expect_equal(
        colnames(p2$constraints$L),
        attr(p2, "varnames")
    )
    expect_equal(
        dim(p2$constraints$Q[[1]]),
        c(5, 5)
    )
    expect_equal(
        dimnames(p2$constraints$Q[[1]]),
        list(attr(p2, "varnames"), attr(p2, "varnames"))
    )

    p3 <- p2 |> lp_constraint(x[1:2] <= 4*z[1:2])

    expect_equal(
        dim(p3$constraints$L),
        c(5, 5)
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
        info2 <- lpsugar_attributes(p2$constraints)
        all(info2$id == c("second", "second", ""))
    })
    expect_true({
        p3 <- p |> lp_delete_constraint(c("first", "second"))
        info3 <- lpsugar_attributes(p3$constraints)
        info3$id == ""
    })

    expect_warning(
        p |> lp_delete_constraint(c("second", "third", "fourth")),
        'Ignoring constraints: "third" and "fourth"'
    )
    expect_error(
        p |> lp_delete_constraint(c("second", "#unnamed_constraint")),
        "Cannot delete unnamed constraints."
    )
    expect_error(
        p |> lp_delete_constraint(c("second", "")),
        "Cannot delete unnamed constraints."
    )
})

test_that("non constraint", {
    p <- lp_problem() |>
        lp_var(x)

    expect_error(
        p |> lp_constraint(1 <= 2),
        "does not contain any variables"
    )
    expect_error(
        p |> lp_constraint(2*x),
        "did not evaluate to a constraint"
    )
    expect_error(
        p |> lp_constraint(my_con = 2*x),
        "Problematic constraint: 'my_con'"
    )
    expect_error(
        p |> lp_constraint(my_con = for (i in 0:3) 2*x),
        "Problematic constraint: 'my_con\\[i=0\\]'"
    )
    expect_error(
        p |> lp_constraint(x != 0),
        "Not equal"
    )
})

test_that("indexing constraints", {
    # Linear --------------
    
    p <- lp_problem() |> 
        lp_var(x[1:5]) |> 
        lp_con(
            vectorized = x * (2:6) >= 4:8,
            fsplit = for (i in 1:5) {
                x[i] / i <= i - 1
            }
        )

    expect_equal(
        p$constraints[1:3],
        p$constraints[1:3, ]
    )
    expect_equal(
        p$constraints["vectorized", ][2],
        p$constraints["vectorized"][2, ]
    )

    expect_equal(
        p$constraints["vectorized"] |> head(2),
        p$constraints["vectorized"][1:2]
    )

    expect_equal(
        p$constraints[c("vectorized", "fsplit")],
        p$constraints
    )

    expect_error(p$constraints[], "Index constraints with")
    expect_error(p$constraints[, 1], "Index constraints with")
    expect_error(p$constraints[1, 1], "Index constraints with")
    expect_error(p$constraints[1, , ], "Index constraints with")
    expect_error(
        p$constraints[c("nope", "vectorized", "neither")],
        'Undefined constraints: "nope" and "neither"'
    )
    
    # Quadratic ----------
    
    q <- lp_problem() |> 
        lp_var(x[1:4]) |> 
        lp_con(
            lc = 2*x + x[1] <= 50,
            qc = x[1:3] * x[2:4] >= 5
        )
    
    expect_no_error({
        q$constraints["lc"]
        q$constraints["qc"]
        q$constraints[c("lc", "qc")]  
    })

    # Nonlinear ---------
    
    nlp <- q |> 
        lp_con(
            nl = nonlinear(log(x) + x) >= 5
        )
    
    expect_no_error({
        nlp$constraints["nl"]
        nlp$constraints[c("lc", "nl")]
    })
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
        "`bind_cons\\(\\)` can only bind <lp_constraint>, not <lp_variable>"
    )
    expect_error(
        rbind(0, x == 1),
        "`bind_cons\\(\\)` can only bind <lp_constraint>, not <numeric>"
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
