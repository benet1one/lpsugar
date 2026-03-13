
# Wrappers -------------------

#' @export
Ops.lp_variable <- function(e1, e2) {
    op <- .Generic
    op_text <- if (rlang::is_missing(e2)) {
        paste0(
            op,
            rlang::enexpr(e1) |> format()
        )
    } else {
        paste(
            rlang::enexpr(e1) |> format(),
            op,
            rlang::enexpr(e2) |> format()
        )
    }

    call <- if (length(op_text) == 1L) {
        str2lang(op_text)
    } else {
        parent.frame()
    }

    # Arithmetic and Logic ---------------
    # +x, -x, !x
    if (rlang::is_missing(e2)) {
        if (op == "+") {
            return(e1)
        } else if (op == "-") {
            return(minus_v(e1))
        } else if (op == "!") {
            return(negate_v(e1, call))
        }
        abort("Unsupported operation `{op}`", call = call)
    }

    # Error if anyNA
    check_no_na(e1, e2, call)

    # Two element arithmetic
    if (op == "+") {
        return(add_lp(e1, e2, call))
    } else if (op == "-") {
        return(subtract_lp(e1, e2, call))
    } else if (op == "*") {
        return(multiply_lp(e1, e2, call))
    } else if (op == "/") {
        return(divide_lp(e1, e2, call))
    } else if (op == "^") {
        return(power_lp(e1, e2, call))
    }

    # Comparison -----------------------
    comparison_ops <- c("<", "<=", "==", ">=", ">")
    if (op %in% comparison_ops) {
        return(compare_lp(e1, e2, op, call))
    } else if (op == "!=") {
        abort("Inequality `!=` is not supported in constraints.", call = call)
    }

    abort("Unsupported operation `{op}`", call = call)
}

check_no_na <- function(e1, e2, call) {
    if (!is_lp_variable(e1) && anyNA(e1)) {
        abort("Left-hand-side object contains NA values.", call = call)
    }
    if (!is_lp_variable(e2) && anyNA(e2)) {
        abort("Right-hand-side object contains NA values.", call = call)
    }
}

# Arithmetics -----------------------

add_lp <- function(x, y, call) {
    xv <- is_lp_variable(x)
    yv <- is_lp_variable(y)

    if (xv && yv) {
        add_v_v(x, y, call)
    } else if (xv) {
        add_v_c(x, y, call)
    } else if (yv) {
        add_v_c(y, x, call)
    } else {
        abort("None are lp_variables", call = call)
    }
}
subtract_lp <- function(x, y, call) {
    xv <- is_lp_variable(x)
    yv <- is_lp_variable(y)

    if (xv && yv) {
        subtract_v_v(x, y, call)
    } else if (xv) {
        subtract_v_c(x, y, call)
    } else if (yv) {
        subtract_c_v(x, y, call)
    } else {
        abort("None are lp_variables", call = call)
    }
}
multiply_lp <- function(x, y, call) {
    xv <- is_lp_variable(x)
    yv <- is_lp_variable(y)

    if (xv && yv) {
        multiply_v_v(x, y, call)
    } else if (xv) {
        multiply_v_c(x, y, call)
    } else if (yv) {
        multiply_v_c(y, x, call)
    } else {
        abort("None are lp_variables", call = call)
    }
}
divide_lp <- function(x, y, call) {
    if (is_lp_variable(y)) {
        divide_a_v(x, y, call)
    } else if (is_lp_variable(x)) {
        divide_v_c(x, y, call)
    } else {
        abort("None are lp_variables", call = call)
    }
}
power_lp <- function(x, y, call) {
    abort("Cannot use powers or exponentials in a linear problem.", call = call)
}

horizontal_multiply <- function(x, c) {
    stopifnot(nrow(x) == length(c))

    for (i in 1:nrow(x)) {
        x[i, ] <- x[i, ] * c[i]
    }

    x
}


# -var
minus_v <- function(x) {
    multiply_v_c(x, -1)
}

# var + var
add_v_v <- function(x, y, call) {
    if (non_conformable(x, y)) {
        abort("Non-conformable arrays", call = call)
    }

    max_n <- max(length(x), length(y))
    x <- recycle_var(x, max_n)
    y <- recycle_var(y, max_n)

    z <- x
    z$coef <- x$coef + y$coef
    z$add <- z$add + y$add
    z$raw <- FALSE

    return(z)
}
# var + constant
add_v_c <- function(x, c, call) {
    if (non_conformable(x, c)) {
        abort("Non-conformable arrays", call = call)
    }

    max_n <- max(length(x), length(c))
    x <- recycle_var(x, max_n)
    c <- recycle_const(c, max_n)

    x$add <- x$add + c
    x$raw <- FALSE

    return(x)
}


# var - var
subtract_v_v <- function(x, y, call) {
    add_v_v(x, minus_v(y), call)
}
# var - constant
subtract_v_c <- function(x, c, call) {
    add_v_c(x, -c, call)
}
# constant - var
subtract_c_v <- function(c, x, call) {
    add_v_c(minus_v(x), c, call)
}

# var * constant
multiply_v_c <- function(x, c, call) {
    if (non_conformable(x, c)) {
        abort("Non-conformable arrays", call = call)
    }

    max_n <- max(length(x), length(c))
    x <- recycle_var(x, max_n)
    c <- recycle_const(c, max_n)

    x$coef <- horizontal_multiply(x$coef, c)
    x$add <- x$add * c
    x$raw <- FALSE

    return(x)
}
# var / constant
divide_v_c <- function(x, c, call) {
    multiply_v_c(x, 1/c, call = call)
}

# Illegal operations:

# var * var
multiply_v_v <- function(x, y, call) {
    abort("Cannot multiply two variables in a linear problem.", call = call)
}
# any / var
divide_a_v <- function(x, y, call) {
    abort("Cannot divide by a variable in a linear problem.", call = call)
}



# Logic ------------------------

negate_v <- function(x, call) {
    if (!x$raw || !x$binary) {
        abort("Negation `!{x$name}` is only supported for binary variables.", call = call)
    }

    # 1 - x
    x$coef <- -x$coef
    x$add <- -x$add + 1
    x$raw <- FALSE

    return(x)
}


# Comparison --------------------

compare_lp <- function(x, y, op, call) {
    if (non_conformable(x, y)) {
        abort("Non-conformable arrays", call = call)
    }

    if (op == "<") {
        op <- "<="
    } else if (op == ">") {
        op <- ">="
    }

    lhs <- x - y
    rhs <- -lhs$add
    lhs <- lhs$coef
    dir <- rep(op, length(rhs))
    call <- rep(format(call), length(rhs))
    name <- character(length(rhs))

    list(lhs = lhs, dir = dir, rhs = rhs, name = name, call = call) |>
        structure(class = "lp_constraint")
}

# Math ------------------------------

#' @export
sum.lp_variable <- function(x, ..., na.rm = FALSE) {
    varnames <- colnames(x$coef)
    x$ind <- x$ind[1]
    x$coef <- colSums(x$coef) |>
        matrix(nrow = 1L) |>
        robust_index()
    x$add <- sum(x$add) |>
        matrix(nrow = 1L, ncol = 1L) |>
        robust_index()

    colnames(x$coef) <- varnames

    if (...length() > 0L) {
        x <- x + sum(..., na.rm = na.rm)
    }

    x$raw <- FALSE
    return(x)
}

#' @export
Math.lp_variable <- function(x, ...) {
    fun <- .Generic
    call <- paste0(fun, "(", format(rlang::enexpr(x)), ")") |>
        str2lang()

    if (fun == "cumsum") {
        return(cumsum_v(x, call))
    }

    if (fun == "abs") {
        abs_url <- "https://optimization.cbe.cornell.edu/index.php?title=Optimization_with_absolute_values"
        message <- glue::glue(
            "Function `abs` is not linear.\n",
            "See how to implement absolute values in linear programming here:\n",
            "{abs_url}"
        )
        abort(message, call = call)
    }

    abort("Function `{fun}` is not supported in a linear problem.", call = call)
}

cumsum_v <- function(x, call) {
    if (length(x) >= 2L) for (i in 2:length(x)) {
        x$coef[i, ] <- x$coef[i, ] + x$coef[i-1L, ]
    }

    x$add[] <- cumsum(x$add)
    x$raw <- FALSE

    return(x)
}

# Custom Data Mask ------------------------

custom_fun <- function() {
    e <- rlang::env()

    e$diag <- function(x = 1, nrow, ncol, names = TRUE) {
        if (!is_lp_variable(x)) {
            return(base::diag(x, nrow, ncol, names))
        }

        warn_changed_args(nrow = , ncol = , names = TRUE)
        diag_v(x)
    }

    e$sum <- function(..., na.rm = FALSE) {
        rlang::dots_list(...) |>
            purrr::map(base::sum, na.rm = na.rm) |>
            purrr::reduce(`+`)
    }

    return(e)
}

diag_v <- function(x) {
    if (ndim(x) != 2L) {
        abort("Variable is not two-dimensional.")
    }

    present_ind <- x$ind
    present_ind[] <- 1:length(present_ind)

    if (!is.matrix(present_ind)) {
        browser()
    }

    present_ind <- base::diag(present_ind)

    x[present_ind]
}

