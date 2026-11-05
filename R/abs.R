
#' @export
abs.lp_variable <- function(x) {
    x$abs <- TRUE
    transformed_variable(x)
}

abs_constraint <- function(x, y, op, call) {
    if (is_abs_variable(x) && !is_abs_variable(y) && op == "<=") {
        lhs <- x
        rhs <- y
    }
    else if (!is_abs_variable(x) && is_abs_variable(y) && op == ">=") {
        lhs <- y
        rhs <- x
    }
    else {
        throw_abs_error(call)
    }

    lhs$abs <- FALSE
    
    bind_cons(
        compare_lp(lhs, +rhs, op = "<=", call = call),
        compare_lp(lhs, -rhs, op = ">=", call = call)
    )
}

check_valid_abs_op <- function(e1, e2, op, call) {
    any_abs <- any(
        is_abs_variable(e1),
        !rlang::is_missing(e2) && is_abs_variable(e2)
    )
    
    if (any_abs && !is.element(op, COMPARISON_OPS)) {
        throw_abs_error(call)
    }
}

throw_abs_error <- function(call = parent.frame()) {
    abs_url <- "https://optimization.cbe.cornell.edu/index.php?title=Optimization_with_absolute_values"
    cli_abort(
        c("Incorrect usage of `abs()`.",
          ">" = "Use it on the left-hand-side of a `<=` comparison: `abs(...) <= ...`",
          "i" = "See how to use absolute values in linear programming here: {.url {abs_url}}"), 
        call = call, 
        class = "lpsugar_error_abs"
    )
}
