
# User -------------------------------

nonlinear <- function(expr) {
    quo <- rlang::enquo(expr)
    as_nonlinear_lp_variable(quo)
}

# Methods ----------------------------

as_nonlinear_lp_variable <- function(x) {
    class(x) <- c(
        "nonlinear_lp_variable",
        "transformed_lp_variable",
        class(x)
    )
    
    return(x)
}

#' @export
print.nonlinear_lp_variable <- function(x, ...) {
    cat(cli::col_grey("<nonlinear_lp_variable>"))
    print(x)
    invisible(x)
}

# Uitls -----------------------------

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

# Functional -----------------------------

#' @export
as.function.nonlinear_lp_variable <- function(nl, problem, ...) {
    check_problem(problem, field_name = "problem")
    
    args <- list(substitute()) |> rep(length(problem$variables))
    names(args) <- names(problem$variables)
    
    expr <- rlang::get_expr(nl)
    env <- rlang::get_env(nl)
    
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
    
    return(fun_x)
}

#' @export
as.function.lp_variable <- function(v, problem, ...) {
    function(x) {
        compute_quadratic(v, x = x)
    }
}

bind_funs <- function(fn_list, problem) {
    fn_list <- purrr::map(fn_list, \(x) as.function(x, problem = problem))
    
    function(x) {
        fn_values <- purrr::map(fn_list, \(fn) fn(x))
        unlist(fn_values)
    }
}

subtract_nl <- function(lhs, rhs) {
    env <- rlang::get_env(lhs)
    lhs_expr <- rlang::get_expr(lhs)
    rhs_expr <- rlang::get_expr(rhs)
    
    subtraction_expr <- rlang::expr({
        .L <- {!!lhs_expr}
        .R <- {!!rhs_expr}
        .L - .R
    })
    
    rlang::new_quosure(subtraction_expr, env = env) |> 
        as_nonlinear_lp_variable()
}

