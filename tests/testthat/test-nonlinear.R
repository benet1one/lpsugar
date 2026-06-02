
test_that("nonlinear", {
    L <- letters[1:3]
    
    fn <- function(x, y, z = 0) {
        x^3 / y["a"] + z
    }
    
    p <- lp_problem() |> 
        lp_var(x) |> 
        lp_var(y[L]) |> 
        lp_minimize_function(fn)
})
