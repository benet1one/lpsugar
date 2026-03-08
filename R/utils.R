
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


for_split <- function(quosure, evaluate = FALSE, data = NULL, recursive = TRUE) {
    if (!rlang::quo_is_symbolic(quosure)) {
        if (evaluate) {
            return(rlang::eval_tidy(quosure, data = data))
        } else {
            return(quosure)
        }
    }

    expr <- rlang::quo_get_expr(quosure)
    env <- rlang::quo_get_env(quosure)

    expr <- inside(expr)

    if (expr[[1]] != quote(`for`)) {
        if (evaluate) {
            return(rlang::eval_tidy(quosure, data = data))
        } else {
            return(quosure)
        }
    }

    variable <- expr[[2L]] |> format()
    sequence <- expr[[3L]] |> rlang::eval_tidy(data = data, env = env)
    interior <- expr[[4L]]

    loop_env <- rlang::new_environment()

    lapply(sequence, function(i) {
        loop_env[[variable]] <- i
        interior_i <- substituteDirect(interior, frame = loop_env)
        quo_i <- rlang::new_quosure(interior_i, env = env)

        if (recursive) {
            return(for_split(
                quo_i,
                evaluate = evaluate,
                data = data,
                recursive = recursive
            ))
        }

        if (evaluate) {
            return(rlang::eval_tidy(quo_id, data = data))
        } else {
            return(quo_i)
        }
    })
}

