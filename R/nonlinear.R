
lp_objective_function <- function(.problem, fun, gradient = NULL, hessian = NULL) {
    expr <- rlang::enquo(fun) |> rlang::as_label()
    
    stopifnot(
        is.function(fun) && !is.primitive(fun),
        is.null(gradient) || is.function(gradient),
        is.null(gradient) || is.function(hessian)
    )
    
    if (!is.null(gradient) || !is.null(hessian)) {
        cli_abort(
            "`gradient` and `hessian` are not yet supported.", 
            call = parent.frame()
        )
    }
    
    check_correct_arguments(fun,      .problem, funname = "fun", call = parent.frame())
    check_correct_arguments(gradient, .problem, funname = "gradient", call = parent.frame())
    check_correct_arguments(hessian,  .problem, funname = "hessian", call = parent.frame())
    
    fun_x      <- recode_arguments(fun,      .problem)
    gradient_x <- recode_arguments(gradient, .problem)
    hessian_x  <- recode_arguments(hessian,  .problem)
    
    n <- ncol(.problem)
    
    fun_sane <- rlang::try_fetch(
        fun_x(rep(0, n)),
        error = identity
    )
    
    if (rlang::is_error(fun_sane)) {
        cli_abort(
            c("Failed to evaluate `fun`.",
              ">" = "Make sure it works when all variables are 0.",
              "i" = "It can return -Inf or +Inf."), 
            call = parent.frame(),
            parent = fun_sane
        )
    }
    
    if (!is.numeric(fun_sane)) {
        cli_abort(
            "`fun` must return a numeric scalar, not `{class(fun_sane)[1]}`",
            call = parent.frame()
        )
    } else if (length(fun_sane) != 1L) {
        cli_abort(
            "`fun` must return a numeric scalar, not of length `{length(fun_sane)}`",
            call = parent.frame()
        )
    }
    
    .problem$objective <- new_nonlinear_objective(
        .problem,
        fun = fun_x,
        gradient = gradient_x,
        hessian = hessian_x,
        expr = expr
    )

    .problem
}

check_correct_arguments <- function(fun, problem, funname, call = parent.frame()) {
    if (is.null(fun)) {
        return()
    }
    
    args <- rlang::fn_fmls_names(fun)
    varnames <- names(problem$variables)
    missing_vars <- varnames[!is.element(varnames, args)]
    
    if (length(missing_vars) > 0L) {
        cli_abort(
            c("`{funname}` must have all problem variables as arguments",
              "x" = "Missing variables: {missing_vars}"),
            call = call,
        )
    }
}

recode_arguments <- function(fun, problem) {
    if (is.null(fun)) {
        return(NULL)
    }
    
    function(x) {
        var_list <- variables_to_list(x, problem, binary_as_logical = FALSE)
        do.call(fun, var_list)
    }
}

new_nonlinear_objective <- function(.problem, direction = NULL, 
                                    fun = NULL, gradient = NULL, hessian = NULL, expr = "") {
    if (is.null(direction)) {
        direction <- .problem$objective$direction
    }
    
    list(
        type = "nonlinear",
        direction = direction,
        fun = fun,
        gradient = gradient,
        hessian = hessian,
        add = 0,
        expr = expr
    ) |> structure(class = "lp_objective")
}

# User -----------------

#' Set a General Nonlinear Objective Function
#' 
#' Optimize an R function.
#'
#' @param .problem An [lp_problem()].
#' @param fun Function to optimize. The function's arguments must match all
#' defined [variables][lp_variable()].
#' @param gradient Function that returns the gradient vector.
#' @param hessian Function that returns the hessian matrix.
#'
#' @seealso [lp_minimize()] to optimize linear or quadratic functions.
#'
#' @returns An [lp_problem()] with the new `$objective` function.
#' 
#' @rdname lp_objective_function
#' @export
#'
#' @example inst/examples/example_nonlinear.R
lp_minimize_function <- function(.problem, fun, gradient = NULL, hessian = NULL) {
    check_problem(.problem)
    .problem$objective$direction <- "minimize"
    lp_objective_function(.problem, {{fun}}, gradient, hessian)
}

#' @rdname lp_objective_function
#' @export
lp_maximize_function <- function(.problem, fun, gradient = NULL, hessian = NULL) {
    check_problem(.problem)
    .problem$objective$direction <- "maximize"
    lp_objective_function(.problem, {{fun}}, gradient, hessian)
}


# Aliases -----------------------

#' @rdname lp_objective_function
#' @export
lp_min_fun <- lp_minimize_function

#' @rdname lp_objective_function
#' @export
lp_max_fun <- lp_maximize_function

