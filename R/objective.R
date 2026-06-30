
# Internal -------------------------------

# Main function, called by lp_minimize() and lp_maximize()
lp_objective <- function(.problem, objective) {
    quosure <- rlang::enquo(objective)
    objective <- rlang::eval_tidy(quosure, data = data_mask(.problem))
    
    if (is.numeric(objective) && length(objective) == 1L && objective == 0) {
        lp_objective_feasible(.problem)
    }
    else if (is_nonlinear(objective)) {
        lp_objective_nonlinear(.problem, objective)
    } 
    else if (is_lp_variable(objective)) {
        expr <- rlang::get_expr(quosure) |> rlang::as_label()
        lp_objective_quadratic(.problem, objective, expr = expr)
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
}

lp_objective_feasible <- function(.problem) {
    .problem$objective <- new_quadratic_objective(
        .problem,
        type = "feasible"
    )
    
    .problem
}

lp_objective_nonlinear <- function(.problem, objective) {
    .problem$objective <- new_nonlinear_objective(
        .problem,
        type = "nonlinear",
        NL = objective,
        expr = rlang::as_label(objective)
    )
    
    .problem
}

lp_objective_quadratic <- function(.problem, objective, expr = "") {
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
    
    .problem$objective <- new_quadratic_objective(
        .problem,
        type = type,
        Q = objective$Q[[1]],
        L = unclass(objective$L),
        A = unclass(objective$A),
        expr = expr
    )
    
    .problem
}

# Constructors -----------------------------

# lp_objective object constructor for quadratic and linear objectives
new_quadratic_objective <- function(.problem, type, direction = NULL, 
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

new_nonlinear_objective <- function(.problem, type, direction = NULL,
                                    NL = NULL, expr = "") {
    if (is.null(direction)) {
        direction <- .problem$objective$direction
    }
    
    fun <- as.function.nonlinear_lp_variable(NL, .problem)
    
    list(
        type = "nonlinear",
        direction = direction,
        NL = NL,
        fun = fun,
        A = 0, # pretty_solution() always adds A
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

# Adds zeros to Q and L coefficients when a variable is added to the problem
update_objective <- function(.problem) {
    if (.problem$objective$type == "undefined") {
        return(.problem)
    } 
    else if (.problem$objective$type == "nonlinear") {
        .problem$objective$fun <- as.function.nonlinear_lp_variable(
            .problem$objective$NL,
            .problem
        )
        
        return(.problem)
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
