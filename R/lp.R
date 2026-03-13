
#' Create a Linear Problem
#'
#' Use [lp_variable()] to define the variables,
#' [lp_minimize()] or [lp_maximize()] to define the objective function,
#' [lp_constraint()] to add constraints and
#' [lp_solve()] to find the optimum.
#'
#' @returns An `lp_problem` object with fields:
#'   - `$variables` : List of [lp_variable()]s.
#'   - `$objective` : List with information about the objective function,
#'   set with [lp_minimize()] or [lp_maximize()].
#'   - `$constraints` : List of constraints added with [lp_constraint()].
#' @export
#'
#' @examples
lp_problem <- function() {
    list(
        variables = list(),
        constraints = list(),
        objective = list(
            coef = numeric(),
            add = 0,
            direction = "",
            expr = ""
        ) |> structure(class = "lp_objective"),

        # Aka implicit variables (impvar)
        aliases = list(),

        # Must equal length of objective coefficients.
        .nvar = 0L,
        # Names of variables with their respective indices, e.g. "x[A, 2]".
        .varnames = character()

    ) |> structure(class = "lp_problem")
}

#' @export
print.lp_problem <- function(x, ...) {
    cat("<lpsugar Linear Problem>\n\n")

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
        print(x$constraints, compact = TRUE)
    }

    cat("\n")
}
