
nonlinear <- function(expr) {
    quo <- rlang::enquo(expr)
    
    class(quo) <- c(
        "nonlinear_lp_variable",
        "transformed_lp_variable",
        "lp_variable",
        class(quo)
    )
    
    return(quo)
}

print.nonlinear_lp_variable <- function(x, ...) {
    cat("<nonlinear_lp_variable>")
    print(x)
    invisible(x)
}

nonlinear_to_function <- function(nonlinear_variable, problem) {
    check_problem(problem, field_name = "problem")
    
    args <- rep(rlang::missing_arg(), length(problem$variables))
    names(args) <- names(problem$variables)
    
    expr <- rlang::get_expr(nonlinear_variable)
    env <- rlang::get_env(nonlinear_variable)
    
    fun <- rlang::new_function(
        args = args,
        body = expr,
        env = env
    )
    
    fun_x <- function(x) {
        vars <- variables_to_list(x, problem)
        do.call(fun, args = vars)
    }
    
    fun_out <- check_function_sanity_new(
        fun_x, 
        n0 = ncol(problem), 
        call = expr
    )
    
    if (!is.numeric(fun_out)) {
        cli_abort(
            c("`fun` must return a numeric vector.",
              "x" = "Returns {.type {fun_out}}."),
            class = "lpsugar_error_nonlinear_not_numeric",
            call = expr
        )
    }
}

check_function_sanity_new <- function(fun_x, n0, call) {
    fun_out <- rlang::try_fetch(
        fun_x(rep(0, n0)),
        error = identity
    )
    
    if (rlang::is_error(fun_out)) {
        cli_abort(
            c("Failed to evaluate expression.",
              ">" = "Make sure it works when all variables are 0.",
              "i" = "It can return -Inf or +Inf."),
            class = "lpsugar_error_nonlinear_throws_error",
            parent = fun_out,
            call = call
        )
    }
    
    return(fun_out)
}
