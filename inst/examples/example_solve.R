library(ROI)

r <- letters[1:2]
c <- LETTERS[1:3]

# Problem with Optimal Solution
p_opt <- lp_problem() |>
    lp_variable(x[r, c], binary = TRUE) |>
    lp_alias(total = sum(x)) |>
    lp_maximize(total + 1) |>
    lp_constraint(x[1, 1] + x[2, 1] <= 1L)

s1 <- lp_solve(p_opt)
s1$status
s1$objective
s1$aliases$total
s1$variables$x

s2 <- lp_solve(p_opt, binary_as_logical = TRUE)
s2$variables$x


# Problem with Unbounded Solution
p_unb <- lp_problem() |>
    lp_variable(y[1:2]) |>
    lp_min(y[1])

s3 <- lp_solve(p_unb)
s3$status
s3$variables$y

# Infeasible Problem
p_no <- lp_problem() |>
    lp_variable(z[letters]) |>
    lp_constraint(z[1] <= z[1] - 10)

s5 <- lp_find_feasible(p_no)

s5$status
s5$variables$z
s5$objective
