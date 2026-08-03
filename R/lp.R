
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
        constraints = empty_constraint(n = 0),
        objective = empty_objective(),
        maximum = NA,
        
        # Aka implicit variables (impvar)
        aliases = list()
        
    ) |> structure(
        class = "lp_problem",
        n_variables = 0L, # Must equal length of objective coefficients.
        varnames = character() # Names of variables with their respective indices, e.g. "x[A, 2]".
    )
}

# Methods ---------------------------

#' @export
print.lp_problem <- function(x, full = FALSE, ...) {
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
    
    obj_info <- lpsugar_attributes(x$objective)
    
    if (obj_info$type == "feasible") {
        print_field(x, "objective")
    }
    else if (obj_info$type != "undefined") {
        direction <- ifelse(x$maximum, "maximize", "minimize")
        print_field_name("objective")
        cat(direction, " ", sep = "")
        print(x$objective)
    }
    
    if (length(x$constraints) > 0L) {
        print_field(x, "constraints", full = full)
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

#' @importFrom ROI `maximum<-`
#' @export
`maximum<-.lp_problem` <- function(x, value) {
    valid <- any(
        length(value) == 1L && is.na(value),
        rlang::is_scalar_logical(value)
    )
    
    if (!valid) {
        cli_abort(
            "`maximum` must be either `TRUE` or `FALSE`.",
            class = "lpsugar_error_bad_maximum_assignment"
        )
    }
    
    x[["maximum"]] <- value
    return(x)
}
