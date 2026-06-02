
lp_objective_function <- function(.problem, fun, gradient = NULL, hessian = NULL) {
    stopifnot(
        is.function(fun) && !is.primitive(fun),
        is.null(gradient) || is.function(gradient),
        is.null(gradient) || is.function(hessian)
    )
    
    check_correct_arguments(fun,      .problem$variables, funname = "fun")
    check_correct_arguments(gradient, .problem$variables, funname = "gradient")
    check_correct_arguments(hessian,  .problem$variables, funname = "hessian")
    
    fun_x      <- recode_arguments(fun,      .problem$variables)
    gradient_x <- recode_arguments(gradient, .problem$variables)
    hessian_x  <- recode_arguments(hessian,  .problem$variables)
    
    n <- ncol(.problem)
    
    fun_sane <- rlang::try_fetch(
        fun_x(rep(0, n)),
        error = identity
    )
    
    if (rlang::is_error(fun_sane)) {
        rlang::abort(
            c("Failed to evaluate `fun`.",
              ">" = "Make sure it works when all variables are 0."), 
            call = parent.frame(),
            parent = fun_sane
        )
    }
    
    if (!is.numeric(fun_sane)) {
        abort("`fun` must return a numeric scalar, not `{class(fun_sane)[1]}`")
    } else if (length(fun_sane) != 1L) {
        abort("`fun` must return a numeric scalar, not of length `{length(fun_sane)}`")
    }
    
    .problem$objective$fun <- fun_x
    .problem$objective$gradient <- gradient_x
    .problem$objective$hessian <- hessian_x
    .problem$objective$expr <- rlang::enexpr(fun) |> rlang::as_label()
    
    .problem
}

check_correct_arguments <- function(fun, variables, funname) {
    if (is.null(fun)) {
        return()
    }
    
    args <- rlang::fn_fmls_names(fun)
    varnames <- names(variables)
    missing_vars <- varnames[!is.element(varnames, args)]
    
    if (length(missing_vars) > 0L) {
        missing_vars_str <- paste0("`", missing_vars, "`", collapse = ", ")
        rlang::abort(c(
            glue::glue("`{funname}` must have all problem variables as arguments"),
            "x" = glue::glue("Missing variables: {missing_vars_str}")
        ))
    }
}

recode_arguments <- function(fun, variables) {
    if (is.null(fun)) {
        return(NULL)
    }
    
    function(x) {
        var_list <- variables_to_list(x, variables, binary_as_logical = FALSE)
        do.call(fun, var_list)
    }
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
#' @examples
lp_minimize_function <- function(.problem, fun, gradient = NULL, hessian = NULL) {
    check_problem(.problem)
    .problem$objective$direction <- "minimize"
    lp_objective_function(.problem, fun, gradient, hessian)
}

#' @rdname lp_objective_function
#' @export
lp_maximize_function <- function(.problem, fun, gradient = NULL, hessian = NULL) {
    check_problem(.problem)
    .problem$objective$direction <- "maximize"
    lp_objective_function(.problem, fun, gradient, hessian)
}
