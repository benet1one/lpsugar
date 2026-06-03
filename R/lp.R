
#' Create a Linear Problem
#'
#' Use [lp_variable()] to define the variables,
#' [lp_minimize()] or [lp_maximize()] to define the objective function,
#' [lp_constraint()] to add constraints and
#' [lp_solve()] to find the optimum.
#'
#' @returns An `lp_problem` object with fields:
#'   - `$variables` : List of variables defined with [lp_variable()].
#'   - `$objective` : List with information about the objective function,
#'   set with [lp_minimize()] or [lp_maximize()].
#'   - `$constraints` : List of constraints added with [lp_constraint()].
#' @export
#'
#' @example inst/examples/example_problem.R
lp_problem <- function() {
    list(
        variables = list(),
        constraints = empty_constraint(),
        objective = list(
            type = "undefined",
            direction = "",
            expr = ""
        ) |> structure(class = "lp_objective"),

        # Aka implicit variables (impvar)
        aliases = list()

    ) |> structure(
        class = "lp_problem",
        n_variables = 0L, # Must equal length of objective coefficients.
        varnames = character() # Names of variables with their respective indices, e.g. "x[A, 2]".
    )
}

#' @export
print.lp_problem <- function(x, compact = TRUE, ...) {
    cat(
        cli::col_grey(rep(cli::symbol$en_dash, 2)),
        cli::style_bold(" <lp_problem> "),
        cli::col_grey(rep(cli::symbol$en_dash, 2)),
        "\n\n",
        sep = ""
    )

    if (length(x$variables) > 0L) {
        print_field(x, "variables")
    }

    if (x$objective$direction != "") {
        print_field(x, "objective")
    }

    if (length(x$constraints) > 0L) {
        print_field(x, "constraints", compact = compact)
    }

    invisible(x)
}

#' @export
dim.lp_problem <- function(x) {
    c(
        n_constraints = length(x$constraints),
        n_variables = attr(x, "n_variables")
    )
}

#' @importFrom stats variable.names
#' @export
variable.names.lp_problem <- function(object, ...) {
    attr(object, "varnames")
}
