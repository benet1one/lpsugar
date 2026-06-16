
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
        solver = "nloptr.slsqp",
        start = lp_find_feasible(p, solver = "highs"),
        max_time = 1
    )
    
    with(s$variables, expect_equal(x, 10 - y, tolerance = 0.001))
    with(s$variables, expect_equal(s$objective, sqrt(x) * log(y)))
})

test_that("nonlinear fun errors", {
    f_error <- function(x) {
        if (x == 0) stop("function does not work for x==0")
        else x^3
    }
    f_char <- function(x) {
        paste0(x, "00")
    }
    f_non_scalar <- function(x) {
        rep(2*x, 3)
    }
    f_missing_vars <- function(x) {
        x^3
    }
    
    expect_error(
        lp_problem() |> lp_var(x) |> lp_min_fun(f_error),
        "Failed to evaluate `fun`(.+)Make sure it works when all variables are 0."
    )
    expect_error(
        lp_problem() |> lp_var(x) |> lp_min_fun(f_char),
        "`fun` must return a numeric scalar(.+)Returns a string."
    )
    expect_error(
        lp_problem() |> lp_var(x) |> lp_min_fun(f_non_scalar),
        "`fun` must return a numeric scalar.(.+)Returns a double vector."
    )
    expect_error(
        lp_problem() |> lp_var(x) |> lp_var(y) |> lp_var(z) |> lp_min_fun(f_missing_vars),
        "`fun` must have all problem variables as arguments.(.+)Missing variables: y and z"
    )
})

test_that("nonlinear gradient", {
    f <- function(x, y) {
        2*x + y^2 + 4*x*y
    }
    g <- function(x, y) {
        list(
            y = 2*y + 4*x,
            x = 2 + 4*y
        )
    }
    
    p <- lp_problem() |> 
        lp_var(x, lower = 0, upper = 10) |> 
        lp_var(y, lower = 0, upper = 10) |> 
        lp_max_fun(fun = f, gradient = g)
    
    expect_equal(
        p$objective$gradient(c(5, 4)),
        variables_to_vec(g(5, 4), p)
    )
})

test_that("nonlinear gradient errors", {
    f <- function(x, y) {
        2*x + y^2 + 4*x*y
    }
    
    g_error <- function(x, y) {
        stop("an error")
    }
    g_bad_vec <- function(x, y) {
        c(4, 2, 3, 1)
    }
    g_bad_list <- function(x, y) {
        list(x = 3, y = 4:6)
    }
    g_missing_vars <- function(x) {
        c(2, 2)
    }
    
    p <- lp_problem() |> 
        lp_var(x) |> 
        lp_var(y)
    
    expect_error(
        p |> lp_min_fun(f, g_error),
        "Failed to evaluate `gradient`."
    )
    expect_error(
        p |> lp_min_fun(f, g_bad_vec),
        "Invalid `gradient` output(.+)`problem` has 2 variables but `gradient\\(\\)` is length 4."
    )
    expect_error(
        p |> lp_min_fun(f, g_bad_list),
        regexp = paste(
            "Invalid `gradient` output",
            "It should be a numeric vector or a named list",
            "Length of variable `y` does not match",
            "In `gradient\\(\\)` it.s length 3",
            "In `problem` it.s length 1",
            sep = "(.+)"
        )
    )
    expect_error(
        p |> lp_min_fun(f, g_missing_vars),
        "Missing variables: y"
    )
})
