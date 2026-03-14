
lp_objective <- function(.problem, objective) {
    quosure <- rlang::enquo(objective)
    objective <- rlang::eval_tidy(quosure, data = data_mask(.problem))
    expr <- rlang::quo_squash(quosure) |> format()

    if (is.numeric(objective) && length(objective) == 1L && objective == 0) {
        # inform("Setting objective to 0 and finding feasible solution instead.",
        #        call = parent.frame())

        .problem$objective$coef[] <- 0
        .problem$objective$add[] <- 0
        .problem$objective$expr <- ""
        return(.problem)
    }

    if (!is_lp_variable(objective)) {
        abort("`objective` must either be an expression containing variables.")
    }
    if (length(objective) == 0L) {
        abort("`objective` evaluated to a variable of length 0.")
    }

    if (length(objective) > 1L) {
        if (length(expr) == 1L) {
            inform("Summing variables in objective. Write `sum({expr})` to suppress this message.",
                   call = parent.frame())
        }

        inform("Summing variables in objective. Write `sum(...)` to suppress this message.",
               call = parent.frame())
        objective <- sum(objective)
    }

    .problem$objective$coef[] <- objective$coef
    .problem$objective$add[] <- objective$add
    .problem$objective$expr <- expr
    return(.problem)
}

#' Set an objective function
#'
#' Minimize of maximize an expression.
#'
#' @param .problem An [lp_problem()].
#' @param objective Expression to optimize, which must evaluate to an `lp_variable` object.
#' Alternatively, set `objective = 0` to remove the objective function and let the solver
#' find a feasible solution instead of optimizing.
#'
#' @details
#' If `objective` evaluates to a multivariate variable instead of a scalar, it will
#' apply `sum(objective)` and display a message. Suppress this message by writing
#' the `sum` yourself.
#'
#' @returns The `.problem` with the updated `$objective` function, a list with these fields:
#' - `$coef` : Vector with the coefficients for each variable.
#' - `$add` : Numeric, addend to the final value. It is not used in the solver.
#' - `$direction` : String, goal of the solver. Can be `"minimize"` or `"maximize"`.
#' - `$expr` : String, expression that defined the objective function.
#' @export
#'
#' @rdname lp_objective
lp_minimize <- function(.problem, objective) {
    check_problem(.problem)
    .problem$objective$direction <- "minimize"
    lp_objective(.problem, {{ objective }})
}
#' @rdname lp_objective
#' @export
lp_maximize <- function(.problem, objective) {
    check_problem(.problem)
    .problem$objective$direction <- "maximize"
    lp_objective(.problem, {{ objective }})
}

# Aliases ----------------------

#' @rdname lp_objective
#' @export
lp_min <- lp_minimize
#' @rdname lp_objective
#' @export
lp_max <- lp_maximize

# Methods ----------------------

#' @export
print.lp_objective <- function(x, ...) {
    cat(x$direction, x$expr, "\n\n")
    print(x$coef)

    if (x$add > 0) {
        glue::glue("\n\n(coef*vars + {x$add})") |> cat()
    } else if (x$add < 0) {
        glue::glue("\n\n(coef*vars - {-x$add})") |> cat()
    }

    cat("\n")
}

# Utils ------------------------

update_objective <- function(.problem) {
    total_vars <- .problem$.nvar
    objective_len <- length(.problem$objective$coef)

    .problem$objective$coef <- c(
        .problem$objective$coef,
        numeric(total_vars - objective_len)
    )

    names(.problem$objective$coef) <- .problem$.varnames
    return(.problem)
}
