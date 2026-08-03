
# Internal -------------------------------

# Main function, called by lp_minimize() and lp_maximize()
lp_objective <- function(.problem, objective) {
    quosure <- rlang::enquo(objective)
    objective <- rlang::eval_tidy(quosure, data = data_mask(.problem))
    
    if (is.numeric(objective) && length(objective) == 1L && objective == 0) {
        .problem$objective <- objective_feasible(.problem)
    }
    else if (is_nonlinear(objective)) {
        .problem$objective <- objective_nonlinear(.problem, objective)
    } 
    else if (is_lp_variable(objective)) {
        expr <- rlang::get_expr(quosure) |> rlang::as_label()
        .problem$objective <- objective_quadratic(.problem, objective, expr = expr)
    }
    else {
        cli_abort(
            c("`objective` must be one of",
              ">" = "The number 0, to find any feasible solution.",
              ">" = "An expression containing variables.",
              ">" = "A call to `nonlinear()`",
              "x" = "Instead found {.type {objective}}."),
            call = parent.frame()
        )
    }

    return(.problem)
}

objective_feasible <- function(.problem) {
    new_quadratic_objective(
        .problem,
        type = "feasible"
    )
}

objective_nonlinear <- function(.problem, objective) {
    new_nonlinear_objective(
        .problem,
        type = "nonlinear",
        NL = objective,
        expr = rlang::as_label(objective)
    )
}

objective_quadratic <- function(.problem, objective, expr = "") {
    if (length(objective) == 0L) {
        cli_abort(
            "`objective` evaluated to a variable of length 0.",
            class = "lpsugar_error_bad_objective",
            call = parent.frame(2)
        )
    }
    if (length(objective) > 1L) {
        objective <- sum(objective)
        cli_inform(
            "Summing variables in objective. Write `sum({expr})` to suppress this message.",
            call = parent.frame(2)
        )
        expr <- paste0("sum(", expr, ")")
    }
    
    if (is_quadratic(objective)) {
        type <- "quadratic" 
    } 
    else {
        type <- "linear"
    }
 
    new_quadratic_objective(
        .problem,
        type = type,
        Q = objective$Q[[1]],
        L = unclass(objective$L),
        A = unclass(objective$A),
        expr = expr
    )
}

# Constructors -----------------------------

# lp_objective object constructor for quadratic and linear objectives
new_quadratic_objective <- function(.problem, type, Q = NULL, L = NULL, A = NULL, expr = "") {
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
    
    out <- if (is.null(Q)) {
        ROI::L_objective(L = L)
    }
    else {
        ROI::Q_objective(Q = Q, L = L)
    }

    out$names <- attr(.problem, "varnames")
    class(out) <- c("lp_objective", class(out))
    
    lpsugar_attributes(out) <- list(
        A = A,
        type = type,
        expr = expr
    )

    out
}

new_nonlinear_objective <- function(.problem, type, NL, expr = "") {
    fun <- as.function.nonlinear(NL, .problem)
    fun_out <- attr(fun, "fun_output")
    
    if (length(fun_out) != 1L) {
        cli_abort(
            c("Nonlinear objective function must return a scalar.",
              "x" = "Instead returns a length {length(fun_out)} vector."),
            class = "lpsugar_error_objective_not_scalar",
            call = parent.frame(3)
        )
    }
    
    out <- ROI::F_objective(fun, n = ncol(.problem))
    class(out) <- c("lp_objective", class(out))
    
    lpsugar_attributes(out) <- list(
        A = 0,
        type = "nonlinear",
        expr = expr
    )

    out
}

empty_objective <- function() {
    structure(
        list(),
        class = c("lp_empty_objective", "lp_objective"),
        lpsugar_attributes = list(
            type = "undefined",
            expr = ""
        )
    )
}

# User -------------------------------

#' Set an Objective Function
#'
#' Minimize of maximize a linear or quadratic expression.
#'
#' @param .problem An [lp_problem()].
#' @param objective Expression to optimize. Can be:
#' - The number 0, in which case the solver will attempt to find any feasible solution.
#' [lp_find_feasible()] serves the same purpose.
#' - A linear or quadratic expression containing decision variables.
#' - A nonlinear expression wrapped in [nonlinear()].
#'
#' @details
#' If `objective` evaluates to a multivariate variable instead of a scalar, it will
#' apply `sum(objective)` and display a message. Suppress this message by writing
#' the `sum` yourself.
#'
#' @returns The `.problem` with the new `$objective` function.
#' 
#' The `$objective` inherits from [ROI::L_objective()], [ROI::Q_objective()],
#' or [ROI::F_objective()].
#' 
#' - A quadratic objective function is represented as
#' 
#'   \eqn{\frac{1}{2} x'Qx + Lx}
#'   
#' - While a nonlinear objective function is simply represented as
#' 
#'   \eqn{F(x)}
#' 
#' @export
#' @seealso [nonlinear()] For general nonlinear optimization.
#' 
#' @rdname lp_objective
#' @example inst/examples/example_objective.R
lp_minimize <- function(.problem, objective) {
    check_problem(.problem)
    .problem$maximum <- FALSE
    lp_objective(.problem, {{ objective }})
}
#' @rdname lp_objective
#' @export
lp_maximize <- function(.problem, objective) {
    check_problem(.problem)
    .problem$maximum <- TRUE
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
    info <- lpsugar_attributes(x)
    
    if (info$type == "undefined") {
        cat("no objective function\n\n")
        return(invisible(x))
    }
    
    if (info$type == "feasible") {
        cat("find a feasible solution\n\n")
        return(invisible(x))
    }
    
    cat(
        info$type, " function:\n",
        info$expr, "\n\n", 
        sep = ""
    )
    invisible(x)
}

# Utils ------------------------

# Adds zeros to Q and L coefficients when a variable is added to the problem
update_objective <- function(.problem) {
    info <- lpsugar_attributes(.problem$objective)
    
    if (info$type == "undefined") {
        return(.problem)
    }
    
    varnames <- variable.names(.problem)
    n_before <- ncol(.problem$objective$L)
    n_after <- ncol(.problem)
    
    if (is_quadratic(.problem$objective)) {
        .problem$objective$Q$nrow <- n_after
        .problem$objective$Q$ncol <- n_after
        .problem$objective$Q$dimnames <- list(varnames, varnames)
    }

    .problem$objective$L <- cbind(
        .problem$objective$L,
        matrix(0, nrow = 1, ncol = n_after - n_before)
    )
    
    colnames(.problem$objective$L) <- varnames
    .problem$objective$names <- varnames
    
    return(.problem)
}
