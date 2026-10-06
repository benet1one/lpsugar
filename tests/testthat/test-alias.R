
test_that("alias", {
    withr::local_package("ROI.plugin.highs")

    p <- lp_problem() |>
        lp_var(x[1:2, 1:3], lower = matrix(1:6, nrow = 2))

    dx <- p |>
        lp_alias(dx = diag(x)) |>
        lp_min(sum(x)) |>
        lp_solve() |>
        _$aliases$dx

    expect_equal(c(dx), c(1, 4))

    expect_error(
        lp_alias(p, sum(x)),
        "must be named"
    )

    expect_message(
        p2 <- p |>
            lp_alias(s = x[1]) |>
            lp_alias(s = x[2]),
        "Overriding alias `s`"
    )
    expect_true(
        all(p2$aliases$s$L == c(0, "x[2,1]" = 1, 0, 0, 0, 0))
    )

    expect_error(
        p |> lp_alias(x = x[1]),
        "Cannot override variable `x`"
    )
    expect_error(
        p |> lp_alias(y = 1:3),
        "Alias `y` did not evaluate to a variable"
    )
    expect_error(
        p |> lp_alias(z = nonlinear(x[1,1]^3)),
        "cannot be `nonlinear"
    )
})

test_that("new alias", {
    A <- letters[1:3]
    B <- LETTERS[1:2]
    
    p <- lp_problem() |> 
        lp_var(x[A, B]) |> 
        lp_impvar_manual(
            y[A], {
                y[] <- 5
                for (a in 1:2) {
                    y[a] = sum(x[a, ])
                }
            }
        )
    
    expect_snapshot(unclass(p$aliases$y))

    p2 <- lp_problem() |> 
        lp_var(x[A, A]) |> 
        lp_alias_manual(
            z[A, A],
            for (i in A) {
                z[, i] <- x[i, i] + 2
            }
        )
    
    expect_snapshot(unclass(p2$aliases$z))
    
    p3 <- lp_problem() |> 
        lp_var(x[A]) |> 
        lp_alias_manual(
            y[A], {
                for (a in 1:2) y[a] = 2*x[a]
                y[3] = sum(x)^2
            }
        )
    
    expect_snapshot(unclass(p3$aliases$y))
    
    expect_error(
        lp_problem() |> 
            lp_var(x) |> 
            lp_alias_manual(d[A, A, A], d[1, 1, 1] <- 1),
        paste(
            "Alias is not fully defined.",
            "26 unassigned values.",
            "First unassigned value at \\(2, 1, 1\\).",
            sep = ".*"
        )
    )
})
