
lp_objective_function <- function(.problem, fun, gradient = NULL, hessian = NULL) {
    expr <- rlang::enquo(fun) |> rlang::as_label()
    
    stopifnot(
        is.function(fun) && !is.primitive(fun),
        is.null(gradient) || is.function(gradient),
        is.null(hessian)  || is.function(hessian)
    )
    
    if (!is.null(hessian)) {
        cli_abort(
            "`hessian` is not yet supported.", 
            call = parent.frame()
        )
    }
    
    check_correct_arguments(fun,      .problem, funname = "fun", call = parent.frame())
    check_correct_arguments(gradient, .problem, funname = "gradient", call = parent.frame())
    check_correct_arguments(hessian,  .problem, funname = "hessian", call = parent.frame())
    
    fun_x <- nl_recode_fun(fun, .problem, call = parent.frame())
    gradient_x <- nl_recode_gradient(gradient, .problem, call = parent.frame())
    
    # TODO
    hessian_x <- NULL
    
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
    
    aliasnames <- names(problem$aliases)
    present_aliases <- aliasnames[is.element(aliasnames, args)]
    
    if (length(missing_vars) > 0L) {
        cli_abort(
            c("`{funname}` must have all problem variables as arguments.",
              "x" = "Missing variables: {missing_vars}"),
            call = call,
        )
    }
    
    if (length(present_aliases) > 0L) {
        cli_abort(
            c("Aliases can not be passed to `{funname}`.",
              ">" = "Only variables can be passed to the objective function.",
              "x" = "Problematic arguments: {present_aliases}"),
            call = call
        )
    }
}

nl_recode_fun <- function(fun, problem, call) {
    fun_x <- recode_arguments(fun, problem)
    fun_out <- check_function_sanity(
        fun_x, 
        n0 = ncol(problem), 
        funname = "fun", 
        call = call
    )
    
    if (!is.numeric(fun_out) || length(fun_out) != 1L) {
        cli_abort(
            c("`fun` must return a numeric scalar.",
              "x" = "Returns {.type {fun_out}}."),
            call = call
        )
    }
    
    return(fun_x)
}

nl_recode_gradient <- function(gradient, problem, call) {
    if (is.null(gradient)) {
        return(NULL)
    }
    
    gradient_x <- recode_arguments(gradient, problem)
    gradient_out <- check_function_sanity(
        gradient_x,
        n0 = ncol(problem),
        funname = "gradient",
        call = call
    )
    
    # Checks for errors, such as missing values, wrong names...
    gradient_out_vec <- rlang::try_fetch(
        variables_to_vec(
            gradient_out,
            problem = problem,
            field = "gradient()"
        ),
        error = identity
    )
    
    if (rlang::is_error(gradient_out_vec)) {
        cli_abort(
            c("Invalid `gradient` output.",
              ">" = "It should be a numeric vector or a named list."),
            call = call,
            parent = gradient_out_vec
        )
    }
    
    if (is.list(gradient_out)) {
        return(function(x) {
            variables_to_vec.list(gradient_x(x), problem)
        })
    } 
    else {
        return(gradient_x)
    }
}

recode_arguments <- function(fun, problem) {
    function(x) {
        var_list <- variables_to_list(x, problem, binary_as_logical = FALSE)
        do.call(fun, var_list)
    }
}

check_function_sanity <- function(fun_x, n0, funname = "fun", call) {
    fun_out <- rlang::try_fetch(
        fun_x(rep(0, n0)),
        error = identity
    )
    
    if (rlang::is_error(fun_out)) {
        cli_abort(
            c("Failed to evaluate `{funname}`.",
              ">" = "Make sure it works when all variables are 0.",
              "i" = "It can return -Inf or +Inf."), 
            call = call,
            parent = fun_out
        )
    }
    
    return(fun_out)
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
#' defined [variables][lp_variable()]. It must return a numeric scalar.
#' @param gradient Function that returns the gradient vector. It can return one of:
#' - Named list of variables with their respective gradients.
#' - Vector containing the derivative of `fun` with respect to each variable.
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

