
test_that("nonlinear", {
    withr::local_package("ROI.plugin.alabama")
    L <- letters[1:3]
    
    fn <- function(x, y, z = 0) {
        x^3 / y["a"] + z
    }
    
    p <- lp_problem() |> 
        lp_var(x, lower = 2) |> 
        lp_var(y[L], upper = 10) |> 
        lp_minimize_function(fn)
    
    expect_snapshot(p$objective)
    
    s <- lp_solve(
        p, 
        start = list(
            y = c(3, 0, 0),
            x = 4
        )
    )
    
    expect_equal(unname(s$objective), 0.8)
    
    expect_equal(
        compute_objective(p, list(x = 5, y = c(a=1, b=2, c=3))),
        fn(x = 5, y = c(a=1, b=2, c=3))
    )
})

test_that("nonlinear constrained", {
    withr::local_package("ROI.plugin.alabama")
    # TODO
})
