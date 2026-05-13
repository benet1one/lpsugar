
# Wrappers -------------------

#' @export
Ops.lp_variable <- function(e1, e2) {
    op <- .Generic
    call <- call(op, substitute(e1), substitute(e2))

    # Single Element --------------------
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

    # Checks ----------------------------

    # Error if anyNA
    check_no_na(e1, e2, call)

    # Compatible dims
    comp <- compatible_dimensions(e1, e2, drop_dim = TRUE)

    if (!comp) {
        why <- attr(comp, "cnd")
        rlang::abort(why$message, call = call)
    }

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
    z$binary <- FALSE

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
    x$binary <- FALSE

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
    z <- add_v_c(minus_v(x), c, call)

    # If its (1-x) and x is binary, it stays binary
    if (x$binary && all(c == 1)) {
        z$binary <- TRUE
    }

    return(z)
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
    x$binary <- FALSE

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
    if (!x$binary) {
        abort("Negation `!{x$name}` is only supported for binary variables.", call = call)
    }

    # 1 - x
    x$coef <- -x$coef
    x$add <- -x$add + 1
    x$raw <- FALSE
    # x$binary <- FALSE

    return(x)
}

# Matrix Operations -------------

#' @export
`%*%.lp_variable` <- function(x, y) {
    call <- rlang::call2("%*%", substitute(x), substitute(y))
    xv <- is_lp_variable(x)
    yv <- is_lp_variable(y)

    if (xv && yv) {
        abort("Cannot matrix multiply `%*%` two variables.")
    }

    if (xv) {
        matrix_multiply_v_c(x, y, call = call)
    } else {
        t(matrix_multiply_v_c(t(y), t(x), call = call))
    }
}

# var %*% mat
matrix_multiply_v_c <- function(x, y, call = parent.frame()) {
    ptype <- rlang::try_fetch(x$ind %*% y, error = identity)

    if (rlang::is_error(ptype)) {
        abort(ptype$message, call = call)
    }

    if (!is.matrix(y)) {
        y <- matrix(y, ncol = 1L)
    }

    out <- x
    out$ind <- ptype
    out$ind[] <- 1:length(out$ind)

    out$coef <- out$coef[integer(), , drop = TRUE]
    out$add <- out$add[integer(), , drop = TRUE]
    out$raw <- FALSE

    for (j in 1:ncol(y)) for (i in 1:nrow(x)) {
        z <- sum(x[i, ] * y[, j])
        out$coef <- rbind(out$coef, z$coef)
        out$add <- rbind(out$add, z$add)
    }

    out$coef <- robust_index(out$coef)
    out$add <- robust_index(out$add)
    return(out)
}


# Methods -----------------------

#' @export
diff.lp_variable <- function(x, lag = 1L, differences = 1L, ...) {
    stopifnot(
        rlang::is_integerish(lag, n = 1, finite = TRUE),
        rlang::is_integerish(differences, n = 1, finite = TRUE)
    )
    if (lag < 1L || differences < 1L) {
        abort("`lag` and `differences` must be integers >= 1")
    }

    xlen <- length(x)

    if (lag * differences >= xlen) {
        return(x[integer()])
    }

    i1 <- -seq_len(lag)
    y <- x

    for (i in seq_len(differences)) {
        ylen <- length(y)
        y <- y[i1] - y[-ylen:-(ylen - lag + 1L)]
    }

    y
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
    lhs <- slam::as.simple_triplet_matrix(lhs$coef)
    dir <- rep(op, length(rhs))
    call <- rep(format1(call), length(rhs))
    name <- character(length(rhs))

    list(lhs = lhs, dir = dir, rhs = rhs, name = name, call = call) |>
        structure(class = "lp_constraint")
}

# Utils ----------------------

horizontal_multiply <- function(x, c) {
    stopifnot(length(c) == nrow(x) || length(c) == 1L)
    c <- array(c, dim = dim(x))
    x*c
}
