
# Wrappers -------------------

#' @export
Ops.lp_variable <- function(x, y) {
    op <- format(.Generic)

    # Arithmetic and Logic ---------------
    # +x, -x, !x
    if (rlang::is_missing(y)) {
        if (op == "+") {
            return(x)
        } else if (op == "-") {
            return(minus_v(x))
        } else if (op == "!") {
            return(negate_v(x))
        }
        abort("unsupported operation `{op}`")
    }

    if (op == "+") {
        return(add_lp(x, y))
    } else if (op == "-") {
        return(subtract_lp(x, y))
    } else if (op == "*") {
        return(multiply_lp(x, y))
    } else if (op == "/") {
        return(divide_lp(x, y))
    } else if (op == "^") {
        return(power_lp(x, y))
    }

    # TODO
}


# Math -----------------------

add_lp <- function(x, y) {
    xv <- is_lp_variable(x)
    yv <- is_lp_variable(y)

    if (xv && yv) {
        add_v_v(x, y)
    } else if (xv) {
        add_v_c(x, y)
    } else if (yv) {
        add_v_c(y, x)
    } else {
        abort("none are lp_variables")
    }
}
subtract_lp <- function(x, y) {
    xv <- is_lp_variable(x)
    yv <- is_lp_variable(y)

    if (xv && yv) {
        subtract_v_v(x, y)
    } else if (xv) {
        subtract_v_c(x, y)
    } else if (yv) {
        subtract_c_v(x, y)
    } else {
        abort("none are lp_variables")
    }
}
multiply_lp <- function(x, y) {
    xv <- is_lp_variable(x)
    yv <- is_lp_variable(y)

    if (xv && yv) {
        multiply_v_v(x, y)
    } else if (xv) {
        multiply_v_c(x, y)
    } else if (yv) {
        multiply_v_c(y, x)
    } else {
        abort("none are lp_variables")
    }
}
divide_lp <- function(x, y) {
    if (is_lp_variable(y)) {
        divide_a_v()
    } else if (is_lp_variable(x)) {
        divide_v_c(x, y)
    } else {
        abort("none are lp_variables")
    }
}
power_lp <- function(...) {
    abort("cannot use powers or exponentials in a linear problem.")
}

horizontal_multiply <- function(x, c) {
    stopifnot(nrow(x) == length(c))
    out <- array(dim = dim(x))

    for (i in 1:nrow(x)) {
        out[i, ] <- x[i, ] * c[i]
    }

    out
}


# -var
minus_v <- function(x) {
    multiply_v_c(x, -1)
}

# var + var
add_v_v <- function(x, y) {
    if (non_conformable(x, y)) {
        abort("non-conformable arrays", call = parent.frame())
    }

    max_n <- max(length(x), length(y))
    x <- recycle_var(x, max_n)
    y <- recycle_var(y, max_n)

    if (!all(dim(x) == dim(y)))
        browser()

    z <- x
    z$coef <- x$coef + y$coef
    z$add <- z$add + y$add
    z$raw <- FALSE

    return(z)
}
# var + constant
add_v_c <- function(x, c) {
    if (non_conformable(x, c)) {
        abort("non-conformable arrays", call = parent.frame())
    }

    max_n <- max(length(x), length(c))
    x <- recycle_var(x, max_n)
    c <- recycle_const(c, max_n)

    x$add <- x$add + c
    x$raw <- FALSE

    return(x)
}


# var - var
subtract_v_v <- function(x, y) {
    add_v_v(x, minus_v(y))
}
# var - constant
subtract_v_c <- function(x, c) {
    add_v_c(x, -c)
}
# constant - var
subtract_c_v <- function(c, x) {
    add_v_c(minus_v(x), c)
}

# var * constant
multiply_v_c <- function(x, c) {
    if (non_conformable(x, c)) {
        abort("non-conformable arrays", call = parent.frame())
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
divide_v_c <- function(x, c) {
    multiply_v_c(x, 1/c)
}

# Illegal operations:

# var * var
multiply_v_v <- function(...) {
    abort("cannot multiply two variables in a linear problem.")
}
# any / var
divide_a_v <- function(...) {
    abort("cannot divide by a variable in a linear problem.")
}



# Logic ------------------------

negate_v <- function(x) {
    if (!x$raw || !x$binary) {
        abort("negation `!{x$name}` is only supported for binary variables.")
    }

    # 1 - x
    x$coef <- -x$coef
    x$add <- -x$add + 1
    x$raw <- FALSE

    return(x)
}

