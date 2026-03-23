profit   <- c(Phone = 60, Tablet = 20, eBook = 10)
max_made <- c(Phone = 500, Tablet = 300, eBook = 950)
product  <- names(profit)
fix_cost <- 11e3

p <- lp_problem() |>
    lp_variable(made[product], lower = 0, upper = max_made, integer = TRUE) |>
    lp_alias(total_profit = sum(made * profit)) |>
    lp_maximize(total_profit - fix_cost) |>
    lp_constraint(sum(made) <= 1500)

p$objective

library(ROI)
(s <- lp_solve(p))

s$aliases$total_profit
sum(p$objective$coef * s$variables_vec)

s$objective
sum(p$objective$coef * s$variables_vec) + p$objective$add
