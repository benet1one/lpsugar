
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
warn_changed_args <- function(..., env = parent.frame(), call = env) {
    expected <- rlang::enexprs(..., .named = TRUE, .homonyms = "error")

    for (arg in names(expected)) {
        if (!exists(arg, envir = env, inherits = FALSE)) {
            warn("Internal warning: unexistant argument `{arg}`.", call = env)
            next
        }

        if (expected[[arg]] != env[[arg]]) {
            warn("Ignoring argument `{arg}`.", call = call)
        }
    }
}

# Safety ------------------------

format1 <- function(x, ...) {
    y <- format(x, ...)

    if (length(y) == 1L) {
        y
    } else if (endsWith(y[1], "{")) {
        paste(y[1], "... }")
    } else {
        paste(y[1], "...")
    }
}
dim2 <- function(x) {
    if (is.null(dim(x))) {
        length(x)
    } else {
        dim(x)
    }
}
ndim <- function(x, drop = FALSE) {
    if (drop) {
        # Without max(1), scalars would return 0 dimensions
        sum(dim2(x) > 1L) |> max(1)
    } else {
        length(dim2(x))
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

    if (rlang::is_error(attempt)) {
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
is_lp_constraint <- function(x, empty_valid = TRUE) {
    is_con <- inherits(x, "lp_constraint")
    if (empty_valid) {
        is_con
    } else {
        is_con && !is_empty_lp_constraint(x)
    }
}
is_empty_lp_constraint <- function(x) {
    inherits(x, "empty_lp_constraint")
}
is_lp_solution <- function(x) {
    inherits(x, "lp_solution")
}

check_problem <- function(x) {
    if (!is_problem(x)) {
        abort("`.problem` must be an lp_problem.", call = parent.frame())
    }
}

# Evaluation --------------------

data_mask <- function(.problem) {
    fun <- custom_fun()
    var <- rlang::new_environment(.problem$variables, parent = fun)
    als <- rlang::new_environment(.problem$aliases, parent = var)

    rlang::new_data_mask(bottom = als, top = fun)
}
lp_eval <- function(.problem, expr, split_for = FALSE) {
    quosure <- rlang::enquo(expr)
    data <- data_mask(.problem)

    if (split_for) {
        for_split(quosure, data = data)
    } else {
        rlang::eval_tidy(quosure, data = data)
    }
}

inside <- function(expr) {
    if (!rlang::is_symbolic(expr)) {
        return(expr)
    }

    if (rlang::is_quosure(expr)) {
        env <- rlang::quo_get_env(expr)
        expr <- rlang::quo_get_expr(expr)
        expr <- inside(expr)
        return(rlang::new_quosure(expr, env))
    }

    if (rlang::is_symbol(expr)) {
        return(expr)
    }

    if (expr[[1L]] == quote(`(`)) {
        return(expr[[2]])
    }

    if (expr[[1L]] == quote(`{`) && length(expr) == 2L) {
        return(expr[[2]])
    }

    expr
}

