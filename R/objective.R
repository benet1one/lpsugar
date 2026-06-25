
lp_objective <- function(.problem, objective) {
    quosure <- rlang::enquo(objective)
    expr <- rlang::as_label(quosure)
    objective <- rlang::eval_tidy(quosure, data = data_mask(.problem))
    
    if (is.numeric(objective) && length(objective) == 1L && objective == 0) {
        .problem$objective <- new_objective(
            .problem,
            type = "feasible",
            L = NULL,
            A = 0,
            expr = ""
        )
        
        return(.problem)
    }
    
    if (!is_lp_variable(objective)) {
        cli_abort(
            c("`objective` must be an expression containing variables.",
              "i" = "Alternatively, use `lp_minimize(0)` to set all coeficients to 0."),
            class = "lpsugar_error_bad_objective"
        )
    }
    if (length(objective) == 0L) {
        cli_abort(
            "`objective` evaluated to a variable of length 0.",
            class = "lpsugar_error_bad_objective"
        )
    }
    if (length(objective) > 1L) {
        objective <- sum(objective)
        cli_inform(
            "Summing variables in objective. Write `sum({expr})` to suppress this message.",
            call = parent.frame()
        )
        expr <- paste0("sum(", expr, ")")
    }
    
    .problem$objective <- new_objective(
        .problem,
        type = if (is_quadratic(objective)) "quadratic" else "linear",
        Q = objective$Q[[1]],
        L = unclass(objective$L),
        A = unclass(objective$A),
        expr = expr
    )
    
    return(.problem)
}

new_objective <- function(.problem, type, direction = NULL, 
                          Q = NULL, L = NULL, A = NULL, expr = "") {
    if (is.null(direction)) {
        direction <- .problem$objective$direction
    }
    
    if (!is.null(Q)) {
        Q <- slam::as.simple_triplet_matrix(Q)
        Q$dimnames <- list(
            attr(.problem, "varnames"),
            attr(.problem, "varnames")
        )
    }
    
    if (is.null(L)) {
        L <- rep(0, ncol(.problem))
        names(L) <- attr(.problem, "varnames")
    } 
    else {
        L <- drop(L)
    }
    
    if (is.null(A)) {
        A <- 0
    } 
    else {
        A <- drop(A)
    }
    
    list(
        type = type,
        direction = direction,
        Q = Q,
        L = L,
        A = A,
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
#' - `$Q` : If objective function is quadratic, matrix with the quadratic coefficients.
#' - `$L` : Vector with the coefficients for each variable.
#' - `$A` : Numeric, addend to the final value. It is not used in the solver.
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
    } 
    else if (.problem$objective$type == "nonlinear") {
        cli_abort(
            c("Cannot add a variable to a nonlinear problem.",
              ">" = paste(
                  "Add the variable before using", 
                  "`lp_minimize_function()` or `lp_maximize_function()`"
              ),
              ">" = paste(
                  "Or reset the objective function with `lp_minimize(0)`",
                  "before adding the variable"
              )
            ),
            class = "lpsugar_error_nonlinear_add_variable"
        )
    }
    
    n_before <- length(.problem$objective$L)
    n_after <- ncol(.problem)
    
    if (is_quadratic(.problem$objective)) {
        .problem$objective$Q$nrow <- n_after
        .problem$objective$Q$ncol <- n_after
        .problem$objective$Q$dimnames <- list(
            attr(.problem, "varnames"),
            attr(.problem, "varnames")
        )
    }
    
    .problem$objective$L <- c(
        .problem$objective$L,
        numeric(n_after - n_before)
    )
    
    names(.problem$objective$L) <- attr(.problem, "varnames")
    return(.problem)
}
