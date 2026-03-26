# Aliases are meant to avoid having to compute the same values
# for multiple different constraints and possibly the objective function.

# Simple Example ----------------------------------

# Say we had x, y, and wanted to define z such that
# z[i] = 2*x[i] + y[n-i+1]

# One option is to use constraints
n <- 3
set <- letters[1:n]
obj_coef <- c(2, 1, 5)

p0 <- lp_problem() |>
    lp_variable(x[set], lower = 1) |>
    lp_variable(y[set], lower = 1) |>
    lp_variable(z[set]) |>
    lp_minimize(sum(z * obj_coef)) |>
    lp_constraint(
        some_constraint = (x + y >= 4),
        constraint_with_z = (z[1] <= 7),
        xyz = for (i in 1:n)  z[i] == 2*x[i] + y[n-i+1]
    )

# But this problem has 3 extra variables and 3 extra constraints:
p0$constraints

# Instead, we can define 'z' as an alias
p1 <- lp_problem() |>
    lp_variable(x[set], lower = 1) |>
    lp_variable(y[set], lower = 1) |>
    lp_alias(
        z = 2*x + rev(y)
    ) |>
    lp_minimize(sum(z * obj_coef)) |>
    lp_constraint(
        some_constraint = x + y >= 4,
        constraint_with_z = (z[1] <= 7)
    )

# This problem no extra variables or constraints!
p1$constraints

# The solution is presented differently
library(ROI) |> suppressMessages()
lp_solve(p0) [c("variables", "aliases")] # z is a variable
lp_solve(p1) [c("variables", "aliases")] # z is an alias



# More Interesting Example ------------------------
Rows <- letters[1:3]
Cols <- LETTERS[1:2]

min_col_cumsum <- c(
    1, 0,
    1, 0,
    5, 3
) |> parameter(Rows, Cols)

max_col_cumsum <- c(
    4, 3,
    5, 9,
    9, 7
) |> parameter(Rows, Cols)


# Without Aliases
p0 <- lp_problem() |>
    lp_var(x[Rows, Cols]) |>
    lp_max(sum(x)) |>
    lp_con(
        min = for (j in Cols) for (i in seq_along(Rows)) {
            sum(x[1:i, j]) >= min_col_cumsum[i, j]
        },
        max = for (j in Cols) for (i in seq_along(Rows)) {
            sum(x[1:i, j]) <= max_col_cumsum[i, j]
        },
        A_less_than_B = for (i in seq_along(Rows)) {
            sum(x[1:i, "A"]) <= sum(x[1:i, "B"])
        }
    )

# With an Alias
p1 <- lp_problem() |>
    lp_var(x[Rows, Cols]) |>
    lp_alias(col_cumsum = apply(x, 2, cumsum)) |>
    lp_max(sum(x)) |>
    lp_con(
        min = (col_cumsum >= min_col_cumsum),
        max = (col_cumsum <= max_col_cumsum),
        A_less_than_B = (col_cumsum[, "A"] <= col_cumsum[, "B"])
    )

# The problems are equivalent
p0$constraints
p1$constraints

# Notice it's not a variable, thus not adding complexity to the problem
p1$variables
# Instead you can find it in $aliases
p1$aliases

# Aliases are computed from the final solution
library(ROI) |> suppressMessages()
s <- lp_solve(p1)
s$variables
s$aliases
