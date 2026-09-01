
test_that("linear fixed", {
    withr::local_package("ROI.plugin.highs")
    
    p <- lp_problem() |> 
        lp_var(x[1:3], lower = 5, upper = c(5, 10, 15)) |> 
        lp_min(sum(x)) |> 
        lp_con(diff(x) >= 3)
    
    expect_snapshot(
        p$constraints |> as.array() |> print(quote = FALSE)
    )
    
    s <- lp_solve(p)
    
    expect_equal(
        s$variables$x,
        c(5, 8, 11),
        ignore_attr = TRUE
    )
})

# TODO
# test with quadratic
# test with nonlinear
