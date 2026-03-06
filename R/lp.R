
#' @export
lp_problem <- function() {
    list(
        variables = list(),
        objective = numeric(),
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

    if (any(x$objective != 0L)) {
        cat("-- $objective --\n")
        print(x$objective)
    }

    if (length(x$constraints) > 0L) {
        cat("-- $constraints --")
        print(x$constraints)
    }

    cat("\n")
}
