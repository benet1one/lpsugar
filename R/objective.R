
lp_objective <- function(.problem, objective) {
    .problem$direction <- direction
    # TODO
}

lp_minimize <- function(.problem, objective) {
    .problem$direction <- "minimize"
    lp_objective(.problem, objective)
}
lp_maximize <- function(.problem, objective) {
    .problem$direction <- "maximize"
    lp_objective(.problem, objective)
}
