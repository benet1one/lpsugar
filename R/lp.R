
#' Create a Linear Problem.
#'
#' Use [lp_variable()] to define the variables,
#' [lp_minimize()] or [lp_maximize()] to define the objective function,
#' [lp_constraint()] to add constraints and
#' [lp_solve()] to solve it.
#'
#' @returns An `lp_problem` object with fields:
#' \describe{
#'   \item{variables}{List of [lp_variable()]s.}
#'   \item{objective}{List with information about the objective function.}
#'   \item{constraints}{List of constraints.}
#' }
#' @export
#'
#' @examples
lp_problem <- function() {
    list(
        variables = list(),
        objective = list(coef = numeric(), add = 0, direction = "") |>
            structure(class = "lp_objective"),
        constraints = list(),

        # Must equal length of objective coefficients.
        .nvar = 0L,
        # Names of variables with their respective indices, e.g. "x[A, 2]".
        .varnames = character()

    ) |> structure(class = "lp_problem")
}

#' @export
print.lp_problem <- function(x, ...) {
    cat("<SugarLP Linear Problem>\n\n")

    if (length(x$variables) > 0L) {
        cat("-- $variables --\n")
        print(x$variables)
    }

    if (any(x$objective$coef != 0L)) {
        cat("-- $objective --\n")
        print(x$objective)
    }

    if (length(x$constraints) > 0L) {
        cat("-- $constraints --")
        print(x$constraints)
    }

    cat("\n")
}
