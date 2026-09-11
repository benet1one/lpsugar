
test_that("linear fixed", {
    withr::local_package("ROI.plugin.highs")
    
    p <- lp_problem() |> 
        lp_var(x[1:3], lower = 5, upper = c(5, 10, 15)) |> 
        lp_min(sum(x)) |> 
        lp_con(diff(x) >= 3)
    
    expect_snapshot(
        p$constraints |> as.array() |> print(quote = FALSE)
    )
    
    s <- lp_solve(p, solver = "highs")
    
    expect_equal(
        s$variables$x,
        c(5, 8, 11),
        ignore_attr = TRUE
    )
})

test_that("quadratic fixed", {
    withr::local_package("ROI.plugin.highs")
    
    p <- lp_problem() |> 
        lp_var(x[1:2, 1:2], lower = 1, upper = matrix(c(4, 3, 1, 5), ncol = 2)) |> 
        lp_alias(
            a1 = 2 * x[1] * x[3],
            a2 = x[2] * (x[3] + 1) - x[4],
            a3 = sum(x^2)
        )
    
    xval <- matrix(c(2, 3, 1, 4), ncol = 2)
    als <- compute_aliases(p, solution = list(x = xval))
    
    expect_equal(
        als$a1,
        2 * xval[1] * xval[3]
    )
    expect_equal(
        als$a2,
        xval[2] * (xval[3] + 1) - xval[4]
    )
    expect_equal(
        als$a3,
        sum(xval^2)
    )
})

test_that("nonlinear fixed", {
    withr::local_package("ROI.plugin.nloptr")
    
    p <- lp_problem() |> 
        lp_var(const, lower = 1, upper = 1) |> 
        lp_var(x[1:4], lower = c(0, 0, 10, 10), upper = c(0, 10, 10, Inf)) |> 
        lp_max(nl(sum(x^3))) |> 
        lp_con(nl(x[2] + sqrt(x[4])) <= 20)
    
    s <- lp_solve(
        p, 
        solver = "nloptr.cobyla", 
        start = list(
            const = 1,
            x = c(0, 5, 10, 15)
        )
    )
    
    expect_equal(
        s$variables_vec,
        variables_to_vec(s$variables, problem = p)
    )
    
    ss <- solution_summary(p, s, tol = 0.2)
    expect_true(ss$feasible)
})

test_that("fixed argument", {
    p <- lp_problem() |> 
        lp_var(x[1:5], lower = 0, upper = 0:4, fixed = c(NA, NA, 1, NA, 3))
    
    expect_equal(
        p$variables$x$ind,
        c(NA, 1, NA, 2, NA),
        ignore_attr = TRUE
    )
})
