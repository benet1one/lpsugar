
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

# Functional -----------------------------

#' @export
as.function.nonlinear <- function(x, problem, ...) {
    check_problem(problem, field_name = "problem")
    nl <- x
    
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

#' @export
as.function.lp_variable <- function(x, problem, ...) {
    variable <- x
    function(x) {
        compute_quadratic(variable, x = x)
    }
}

bind_funs <- function(fn_list, problem) {
    fn_list <- purrr::map(fn_list, \(x) as.function(x, problem = problem))
    
    function(x) {
        fn_values <- purrr::map(fn_list, \(fn) fn(x))
        unlist(fn_values)
    }
}

check_nonlinear_constraint_sanity <- function(nl_con, problem) {
    fun <- as.function.nonlinear(nl_con$NL, problem)
    fun_out <- attr(fun, "fun_output")
    
    lhs_len <- length(fun_out)
    rhs_len <- length(nl_con$rhs)
    
    if (lhs_len == rhs_len) {
        return(fun)
    }
    
    in_con <- if (nl_con$name != "") {
        paste0(" in constraint '", nl_con$name, "'")
    }
    else {
        ""
    }
    
    call <- call(
        nl_con$dir,
        call(nonlinear, nl_con$NL),
        nl_con$rhs
    )
    
    cli_abort(
        c("Length mismatch{in_con}.",
          "x" = "Left-hand-side is length {lhs_len}.",
          "x" = "Right-hand-side is length {rhs_len}."),
        class = "lpsugar_error_nonlinear_constraint_length_mismatch",
        call = call
    )
}
