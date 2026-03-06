
# Misc --------------------------

`%||%` <- rlang::`%||%`
inside <- function(expr) {
    if (rlang::is_symbol(expr)) {
        return(expr)
    }

    stopifnot(rlang::is_symbolic(expr))

    if (expr[[1L]] == quote(`(`)) {
        return(expr[[2]])
    }

    if (expr[[1L]] == quote(`{`)) {
        if (length(expr) > 2) {
            stop("Expression enclosed by `{` has more than one line.")
        } else {
            return(expr[[2]])
        }
    }

    expr
}

warn <- function(message, call = parent.frame(), ...) {
    message <- glue::glue(message, .envir = parent.frame())
    rlang::warn(message = message, call = call, ...)
}
abort <- function(message, call = parent.frame(), ...) {
    message <- glue::glue(message, .envir = parent.frame())
    rlang::abort(message = message, call = call, ...)
}

# Inheritance -------------------

is_prbblem <- function(x) {
    inherits(x, "lp_problem")
}

check_problem <- function(x) {
    if (!is_problem(x))
        abort("`.problem` must be a lp_problem.", call = parent.frame())
}
