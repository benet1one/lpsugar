
# Messages --------------------------

inform <- function(message, call = parent.frame(), ...) {
    message <- glue::glue(message, .envir = parent.frame())
    rlang::inform(message = message, call = call, ...)
}
warn <- function(message, call = parent.frame(), ...) {
    message <- glue::glue(message, .envir = parent.frame())
    rlang::warn(message = message, call = call, ...)
}
abort <- function(message, call = parent.frame(), ...) {
    message <- glue::glue(message, .envir = parent.frame())
    rlang::abort(message = message, call = call, ...)
}

# Safety ------------------------

ndim <- function(x) {
    if (is.null(dim(x))) {
        1L
    } else {
        length(dim(x))
    }
}
non_conformable <- function(x, y) {
    length(x) > 1L && length(y) > 1L && length(x) != length(y)
}
compatible_dimensions <- function(x, y, drop_dim = TRUE) {
    if (is_lp_variable(x)) {
        x <- x$ind
    }
    if (is_lp_variable(y)) {
        y <- y$ind
    }

    if (drop_dim) {
        x <- drop(x)
        y <- drop(y)
    }

    attempt <- rlang::try_fetch(x + y, warning = identity, error = identity)

    if (rlang::is_condition(attempt)) {
        return(structure(FALSE, cnd = attempt))
    } else {
        return(TRUE)
    }
}

# Inheritance -------------------

is_problem <- function(x) {
    inherits(x, "lp_problem")
}
is_lp_variable <- function(x) {
    inherits(x, "lp_variable")
}

check_problem <- function(x) {
    if (!is_problem(x))
        abort("`.problem` must be an lp_problem.", call = parent.frame())
}

# Evaluation --------------------

data_mask <- function(.problem) {
    # TODO Add aliases
    .problem$variables
}

inside <- function(expr) {
    if (rlang::is_quosure(expr)) {
        env <- rlang::quo_get_env(expr)
        expr <- rlang::quo_get_expr(expr)
        expr <- inside(expr)
        return(rlang::new_quosure(expr, env))
    }

    if (rlang::is_symbol(expr)) {
        return(expr)
    }

    stopifnot(rlang::is_symbolic(expr))

    if (expr[[1L]] == quote(`(`)) {
        return(expr[[2]])
    }

    if (expr[[1L]] == quote(`{`) && length(expr) == 2L) {
        return(expr[[2]])
    }

    expr
}


