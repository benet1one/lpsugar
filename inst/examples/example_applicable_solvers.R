library(ROI)

quadratic_prob <- lp_problem() |> 
    lp_var(x, lower = 0) |> 
    lp_var(y, lower = 0) |> 
    lp_min(x^2 + y) |> 
    lp_con(x + y >= 10)

# Installed and loaded quadratic solvers
lpsugar_applicable_solvers(quadratic_prob)
# All quadratic solvers
lpsugar_available_solvers(quadratic_prob) [c("Package", "Version", "Repository")]
