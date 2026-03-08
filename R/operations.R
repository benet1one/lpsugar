
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
    call <- str2lang(op_text)

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
        abort("unsupported operation `{op}`", call = call)
    }

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
        abort("inequality `!=` is not supported in constraints.", call = call)
    }

    abort("unsupported operation `{op}`", call = call)
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
        abort("none are lp_variables", call = call)
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
        abort("none are lp_variables", call = call)
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
        abort("none are lp_variables", call = call)
    }
}
divide_lp <- function(x, y, call) {
    if (is_lp_variable(y)) {
        divide_a_v(x, y, call)
    } else if (is_lp_variable(x)) {
        divide_v_c(x, y, call)
    } else {
        abort("none are lp_variables", call = call)
    }
}
power_lp <- function(x, y, call) {
    abort("cannot use powers or exponentials in a linear problem.", call = call)
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
        abort("non-conformable arrays", call = call)
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
        abort("non-conformable arrays", call = call)
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
        abort("non-conformable arrays", call = call)
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
    abort("cannot multiply two variables in a linear problem.", call = call)
}
# any / var
divide_a_v <- function(x, y, call) {
    abort("cannot divide by a variable in a linear problem.", call = call)
}



# Logic ------------------------

negate_v <- function(x, call) {
    if (!x$raw || !x$binary) {
        abort("negation `!{x$name}` is only supported for binary variables.", call = call)
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
        abort("non-conformable arrays", call = call)
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

    list(lhs = lhs, dir = dir, rhs = rhs, call = format(call)) |>
        structure(class = "lp_constraint")
}

# Math ------------------------------

#' @export

    }


    x$raw <- FALSE

}
