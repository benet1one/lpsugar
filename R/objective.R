
lp_objective <- function(.problem, objective) {
    quosure <- rlang::enquo(objective)
    objective <- rlang::eval_tidy(quosure, data = data_mask(.problem))
    expr <- rlang::as_label(quosure)

    if (is.numeric(objective) && length(objective) == 1L && objective == 0) {
        # inform("Setting objective to 0 and finding feasible solution instead.",
        #        call = parent.frame())

        .problem$objective$q_coef <- NULL
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
        msg_expr <- if (length(expr) == 1L) {
            expr
        } else {
            "..."
        }

        inform("Summing variables in objective. Write `sum({msg_expr})` to suppress this message.",
               call = parent.frame())
        objective <- sum(objective)
    }

    if (is_quadratic(objective)) {
        .problem$objective$q_coef <- objective$q_coef[[1]]
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
#' Alternatively, set `objective = 0` to make the solver find a feasible solution
#' instead of optimizing, just like [lp_find_feasible()] does.
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
#' @example inst/examples/example_objective.R
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
    if (x$direction == "") {
        cat("no objective function\n\n")
        return(invisible(x))
    }

    if (all(x$coef == 0) && !is_quadratic(x)) {
        cat("find a feasible solution\n\n")
        return(invisible(x))
    }

    type <- if (is_quadratic(x)) "quadratic" else "linear"
    cat(x$direction, " ", type, " function:\n",
        x$expr, "\n\n", sep = "")
    invisible(x)
}

# Utils ------------------------

update_objective <- function(.problem) {
    n_before <- length(.problem$objective$coef)
    n_after <- ncol(.problem)

    if (is_quadratic(.problem$objective)) {
        .problem$objective$q_coef <- .problem$objective$q_coef |>
            cbind(matrix(0, nrow = n_before, ncol = n_after - n_before)) |>
            rbind(matrix(0, nrow = n_after - n_before, ncol = n_after))

        colnames(.problem$objective$q_coef) <-
            rownames(.problem$objective$q_coef) <-
            attr(.problem, "varnames")
    }

    .problem$objective$coef <- c(
        .problem$objective$coef,
        numeric(n_after - n_before)
    )

    names(.problem$objective$coef) <- attr(.problem, "varnames")
    return(.problem)
}
