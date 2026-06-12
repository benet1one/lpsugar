
test_that("nonlinear", {
    withr::local_package("ROI.plugin.nloptr")
    L <- letters[1:3]
    
    my_fun <- function(x, y, z = 0) {
        x^3 / y["b"] + z
    }
    
    p <- lp_problem() |> 
        lp_var(x, lower = 2) |> 
        lp_var(y[L], upper = 10) |> 
        lp_minimize_function(my_fun)
    
    expect_snapshot(p$objective)
    
    s <- lp_solve(
        p, 
        solver = "nloptr.cobyla",
        start = list(
            y = c(3, 3, 3),
            x = 4
        )
    )
    
    expect_equal(
        s$objective |> round(6) |> unname(), 
        0.8
    )
    
    expect_equal(
        compute_objective(p, list(x = 5, y = c(a=1, b=2, c=3))),
        my_fun(x = 5, y = c(a=1, b=2, c=3))
    )
    
    expect_error(
        p |> lp_variable(z),
        "Cannot add a variable to a nonlinear problem."
    )
})

test_that("nonlinear constrained", {
    withr::local_package("ROI.plugin.highs")
    withr::local_package("ROI.plugin.nloptr")
    
    p <- lp_problem() |> 
        lp_var(x, lower = 1) |> 
        lp_var(y, lower = 1) |> 
        lp_max_fun(\(x, y) sqrt(x) * log(y)) |> 
        lp_con(x == 10 - y)
    
    s <- lp_solve(
        p,
        solver = "nloptr.isres",
        start = lp_find_feasible(p, solver = "highs")
    )
    
    expect_equal(
        s$variables$x,
        10 - s$variables$y,
        tolerance = 0.001
    )
    
    expect_equal(
        s$objective,
        sqrt(s$variables$x) * log(s$variables$y)
    )
})

test_that("nonlinear errors", {
    error_fun <- function(x) {
        if (x == 0) stop("function does not work for x==0")
        else x^3
    }
    char_fun <- function(x) {
        paste(x, "00")
    }
    non_scalar_fun <- function(x) {
        matrix(2*x, nrow = 3)
    }
    missing_vars_fun <- function(x) {
        x^3
    }
    
    expect_error(
        lp_problem() |> lp_var(x) |> lp_min_fun(error_fun),
        "Failed to evaluate `fun`(.+)Make sure it works when all variables are 0."
    )
    expect_error(
        lp_problem() |> lp_var(x) |> lp_min_fun(char_fun),
        "`fun` must return a numeric scalar, not <character>"
    )
    expect_error(
        lp_problem() |> lp_var(x) |> lp_min_fun(non_scalar_fun),
        "`fun` must return a numeric scalar.(.+)Returns <matrix> of length 3"
    )
    expect_error(
        lp_problem() |> lp_var(x) |> lp_var(y) |> lp_var(z) |> lp_min_fun(missing_vars_fun),
        "`fun` must have all problem variables as arguments.(.+)Missing variables: y and z"
    )
})
