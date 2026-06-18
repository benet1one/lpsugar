# Variable `x` has 5 values
fixed_x <- numeric(5)
# The first value of `x` should be 1, last value should be 9
fixed_x[1] <- 1
fixed_x[5] <- 9
# Other values are free
fixed_x[2:4] <- NA

p <- lp_problem() |> 
    lp_var(x[1:5], lower = 0) |> 
    lp_fix_vars(x = fixed_x)

rbind(
    lower = p$variables$x$lower,
    upper = p$variables$x$upper
)
