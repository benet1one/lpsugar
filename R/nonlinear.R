
# User -------------------------------

#' Nonlinear Expression
#' 
#' Nonlinear transformations and operations on variables.
#' 
#' @param expr (Unquoted) expression containing
#' decision variables. Must return a numeric vector
#' when evaluated.
#' 
#' @export
#' @returns The quoted expression with class
#' `"nonlinear"`
#' @example inst/examples/example_nonlinear.R
nonlinear <- function(expr) {
    quo <- rlang::enquo(expr)
    as_nonlinear(quo)
}

#' @rdname nonlinear
#' @export
nl <- nonlinear

# Methods ----------------------------

as_nonlinear <- function(x) {
    class(x) <- c(
        "nonlinear",
        "transformed_lp_variable",
        class(x)
    )
    
    return(x)
}

#' @export
print.nonlinear <- function(x, ...) {
    cat(cli::col_grey("<nonlinear>\n"))
    print(rlang::get_expr(x))
    invisible(x)
}

# Uitls -----------------------------

check_function_sanity <- function(fun_x, n0, call) {
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

#' @export
as.function.nonlinear <- function(x, problem, ...) {
    check_problem(problem, field_name = "problem")
    nl <- x
    
    args <- list(substitute()) |> rep(length(problem$variables))
    names(args) <- c(
        names(problem$variables),
        names(problem$aliases)
    )
    
    expr <- rlang::get_expr(nl)
    env <- rlang::get_env(nl)
    
    fun <- rlang::new_function(
        args = args,
        body = expr,
        env = env
    )
    
    fun_x <- function(x) {
        vars <- variables_to_list(x, problem)
        als <- compute_aliases(problem, solution = x)
        do.call(fun, args = c(vars, als))
    }
    
    fun_out <- check_function_sanity(
        fun_x, 
        n0 = ncol(problem), 
        call = expr
    )
    
    if (!is.numeric(fun_out)) {
        cli_abort(
            c("Nonlinear expression must return a numeric vector.",
              "x" = "Instead returns {.type {fun_out}}."),
            class = "lpsugar_error_nonlinear_not_numeric",
            call = expr
        )
    }
    
    structure(fun_x, fun_output = fun_out)
}
