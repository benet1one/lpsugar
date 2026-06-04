
lp_objective <- function(.problem, objective) {
    quosure <- rlang::enquo(objective)
    expr <- rlang::as_label(quosure)
    objective <- rlang::eval_tidy(quosure, data = data_mask(.problem))

    if (is.numeric(objective) && length(objective) == 1L && objective == 0) {
        .problem$objective <- new_objective(
            .problem,
            type = "feasible",
            coef = NULL,
            add = 0,
            expr = ""
        )
        
        return(.problem)
    }

    if (!is_lp_variable(objective)) {
        rlang::abort(c(
            "`objective` must be an expression containing variables.",
            "i" = "Alternatively, use `lp_minimize(0)` to set all coeficients to 0."
        ))
    }
    if (length(objective) == 0L) {
        abort("`objective` evaluated to a variable of length 0.")
    }
    if (length(objective) > 1L) {
        objective <- sum(objective)
        inform(
            "Summing variables in objective. Write `sum({expr})` to suppress this message.",
            call = parent.frame()
        )
        expr <- glue::glue("sum({expr})")
    }

    .problem$objective <- new_objective(
        .problem,
        type = if (is_quadratic(objective)) "quadratic" else "linear",
        q_coef = objective$q_coef[[1]],
        coef = unclass(objective$coef),
        add = unclass(objective$add),
        expr = expr
    )
    
    return(.problem)
}

new_objective <- function(.problem, type, direction = NULL, 
                          q_coef = NULL, coef = NULL, add = NULL, expr = "") {
    if (is.null(direction)) {
        direction <- .problem$objective$direction
    }
    
    if (!is.null(q_coef)) {
        q_coef <- slam::as.simple_triplet_matrix(q_coef)
        q_coef$dimnames <- list(
            attr(.problem, "varnames"),
            attr(.problem, "varnames")
        )
    }
    
    if (is.null(coef)) {
        coef <- rep(0, ncol(.problem))
        names(coef) <- attr(.problem, "varnames")
    } else {
        coef <- drop(coef)
    }
    
    if (is.null(add)) {
        add <- 0
    } else {
        add <- drop(add)
    }
    
    list(
        type = type,
        direction = direction,
        q_coef = q_coef,
        coef = coef,
        add = add,
        expr = expr
    ) |> structure(class = "lp_objective")
}

# User -------------------------------

#' Set an Objective Function
#'
#' Minimize of maximize a linear or quadratic expression.
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
#' @returns The `.problem` with the new `$objective` function, a list with these fields:
#' - `$q_coef` : If objective function is quadratic, matrix with the quadratic coefficients.
#' - `$coef` : Vector with the coefficients for each variable.
#' - `$add` : Numeric, addend to the final value. It is not used in the solver.
#' - `$direction` : String, goal of the solver. Can be `"minimize"` or `"maximize"`.
#' - `$expr` : String, expression that defined the objective function.
#' @export
#' @seealso [lp_minimize_function()] For general nonlinear optimization.
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
    if (x$type == "undefined") {
        cat("no objective function\n\n")
        return(invisible(x))
    }

    if (x$type == "feasible") {
        cat("find a feasible solution\n\n")
        return(invisible(x))
    }

    cat(
        x$direction, " ", x$type, " function:\n",
        x$expr, "\n\n", 
        sep = ""
    )
    invisible(x)
}

# Utils ------------------------

update_objective <- function(.problem) {
    if (.problem$objective$type == "undefined") {
        return(.problem)
    } else if (.problem$objective$type == "nonlinear") {
        rlang::abort(c(
            "Cannot add a variable to a nonlinear problem.",
            ">" = paste(
                "Use `lp_variable()` before", 
                "`lp_minimize_function()` or `lp_maximize_function()`"
            )
        ))
    }
    
    n_before <- length(.problem$objective$coef)
    n_after <- ncol(.problem)

    if (is_quadratic(.problem$objective)) {
        .problem$objective$q_coef$nrow <- n_after
        .problem$objective$q_coef$ncol <- n_after
        .problem$objective$q_coef$dimnames <- list(
            attr(.problem, "varnames"),
            attr(.problem, "varnames")
        )
    }

    .problem$objective$coef <- c(
        .problem$objective$coef,
        numeric(n_after - n_before)
    )

    names(.problem$objective$coef) <- attr(.problem, "varnames")
    return(.problem)
}
