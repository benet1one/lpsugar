# ROI or a ROI plugin needs to be loaded for lp_solve() to work
library(ROI) |> suppressMessages()

lp_problem() |>
    lp_variable(x[1:2], lower = 0) |>
    lp_maximize(x[1] + x[2]) |>
    lp_constraint(
        2*x[1] +   x[2] <= 8,
        2*x[1] + 3*x[2] <= 12
    ) |>
    lp_solve()

# For a Quick Start guide see `vignette("lpsugar", package = "lpsugar")`
