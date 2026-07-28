
test_that("nonlinear", {
    withr::local_package("ROI.plugin.nloptr")
    L <- letters[1:3]
    
    obj <- nonlinear(x^3 / y["b"] + z)
    z <- 0
    
    p <- lp_problem() |>
        lp_var(x, lower = 2) |>
        lp_var(y[L], upper = 10) |>
        lp_minimize(obj)

    expect_snapshot(p$objective)

    s <- lp_solve(
        p,
        solver = "nloptr.cobyla",
        start = list(
            x = 4,
            y = c(3, 3, 3)
        )
    )

    expect_equal(
        s$objective |> round(6) |> unname(),
        0.8
    )

    var_values <- list(x = 5, y = c(a=1, b=2, c=3))
    
    expect_equal(
        compute_objective(p, var_values),
        rlang::eval_tidy(obj, data = var_values)
    )

    expect_error(
        pz <- p |> lp_variable(z, lower = -0.5),
        "Cannot add variables to a nonlinear problem"
    )
})

test_that("nonlinear constrained", {
    withr::local_package("ROI.plugin.highs")
    withr::local_package("ROI.plugin.nloptr")

    p <- lp_problem() |>
        lp_var(x, lower = 1) |>
        lp_var(y, lower = 1) |>
        lp_max(nonlinear(sqrt(x) * log(y))) |>
        lp_con(x == 10 - y)

    s <- lp_solve(
        p,
        solver = "nloptr.isres",
        start = lp_find_feasible(p, solver = "highs"),
        max_time = 1
    )

    with(s$variables, expect_equal(x, 10 - y, tolerance = 0.001))
    with(s$variables, expect_equal(s$objective, sqrt(x) * log(y)))
})

test_that("nonlinear constraints", {
    withr::local_package("ROI.plugin.nloptr")
    
    p <- lp_problem() |> 
        lp_var(x) |> 
        lp_var(y) |> 
        lp_var(z[1:3]) |> 
        lp_min(x^2) |> 
        lp_con(
            nonlinear(x/y) >= 1,
            nonlinear(z^3) <= 100
        )
    
    start <- list(x = 6, y = 2, z = 1:3)
    
    s <- lp_solve(
        p, 
        start = start,
        solver = "nloptr.cobyla",
        max_iter = 500
    )
    
    s$status
    
    with(s$variables, {
        expect_true(x^2 < 0.1)
        expect_true(x/y >= 1)
        expect_true(all(z^3 < 100))
    })
    
    cs <- constraint_summary(p, start)
    
    expect_equal(
        cs$lhs,
        c(6/2, (1:3)^3)
    )
})

test_that("operations outside nonlinear", {
    expect_error(
        nonlinear(x + 1) / 2,
        r"(Instead try `nonlinear\(\(x \+ 1\) / 2\)`)"
    )
})
