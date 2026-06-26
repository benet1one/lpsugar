
# Wrappers -------------------

# Wrapping all operations in Ops.lp_variable
# allows better error handling
# even if the code is uglier

#' @export
Ops.lp_variable <- function(e1, e2) {
    op <- .Generic
    call <- call(op, substitute(e1), substitute(e2))
    
    # Single Element --------------------
    # +x, -x, !x
    if (rlang::is_missing(e2)) {
        if (op == "+") {
            return(e1)
        } 
        else if (op == "-") {
            return(minus_v(e1))
        } 
        else if (op == "!") {
            return(negate_v(e1, call))
        }
        cli_abort(
            "Unsupported operation `{op}`", 
            class = "lpsugar_error_unsupported_operation",
            call = call
        )
    }
    
    # Checks ----------------------------
    
    # Error if anyNA
    check_no_na(e1, e2, call)
    
    # Compatible dims
    comp <- compatible_dimensions(e1, e2, drop_dim = TRUE)
    
    if (!comp) {
        why <- attr(comp, "cnd")
        cli_abort(why$message, call = call)
    }
    
    # Two element arithmetic
    if (op == "+") {
        return(add_lp(e1, e2, call))
    } 
    else if (op == "-") {
        return(subtract_lp(e1, e2, call))
    } 
    else if (op == "*") {
        return(multiply_lp(e1, e2, call))
    } 
    else if (op == "/") {
        return(divide_lp(e1, e2, call))
    } 
    else if (op == "^") {
        return(power_lp(e1, e2, call))
    }
    
    # Comparison -----------------------
    comparison_ops <- c("<", "<=", "==", ">=", ">")
    if (op %in% comparison_ops) {
        return(compare_lp(e1, e2, op, call))
    } 
    else if (op == "!=") {
        cli_abort(
            "Not equal `!=` is not supported in constraints.",
            class = "lpsugar_error_not_equal_constraint",
            call = call
        )
    }
    
    cli_abort(
        "Unsupported operation `{op}`", 
        class = "lpsugar_error_unsupported_operation",
        call = call
    )
}

check_no_na <- function(e1, e2, call) {
    if (!is_lp_variable(e1) && anyNA(e1)) {
        cli_abort(
            "Left-hand-side object contains NA values.", 
            class = "lpsugar_error_na_values",
            call = call
        )
    }
    if (!is_lp_variable(e2) && anyNA(e2)) {
        cli_abort(
            "Right-hand-side object contains NA values.", 
            class = "lpsugar_error_na_values",
            call = call
        )
    }
}

# Arithmetics -----------------------

add_lp <- function(x, y, call) {
    xv <- is_lp_variable(x)
    yv <- is_lp_variable(y)
    
    if (xv && yv) {
        add_v_v(x, y, call)
    } 
    else if (xv) {
        add_v_c(x, y, call)
    } 
    else if (yv) {
        add_v_c(y, x, call)
    } 
    else {
        cli_abort("None are lp_variables", call = call)
    }
}
subtract_lp <- function(x, y, call) {
    xv <- is_lp_variable(x)
    yv <- is_lp_variable(y)
    
    if (xv && yv) {
        subtract_v_v(x, y, call)
    } 
    else if (xv) {
        subtract_v_c(x, y, call)
    } 
    else if (yv) {
        subtract_c_v(x, y, call)
    } 
    else {
        cli_abort("None are lp_variables", call = call)
    }
}
multiply_lp <- function(x, y, call) {
    xv <- is_lp_variable(x)
    yv <- is_lp_variable(y)
    
    if (xv && yv) {
        multiply_v_v(x, y, call)
    } 
    else if (xv) {
        multiply_v_c(x, y, call)
    } 
    else if (yv) {
        multiply_v_c(y, x, call)
    } 
    else {
        cli_abort("None are lp_variables", call = call)
    }
}
divide_lp <- function(x, y, call) {
    if (is_lp_variable(y)) {
        divide_a_v(x, y, call)
    } 
    else if (is_lp_variable(x)) {
        divide_v_c(x, y, call)
    } 
    else {
        cli_abort("None are lp_variables", call = call)
    }
}
power_lp <- function(x, y, call) {
    if (is_lp_variable(x) && !is_lp_variable(y)) {
        power_v_c(x, y, call)
    } 
    else {
        cli_abort("Non-quadratic operation.", call = call)
    }
}

# -var
minus_v <- function(x) {
    multiply_v_c(x, -1)
}

# var + var
add_v_v <- function(x, y, call) {
    max_n <- max(length(x), length(y))
    x <- recycle_var(x, max_n)
    y <- recycle_var(y, max_n)
    
    out <- x
    
    qx <- is_quadratic(x)
    qy <- is_quadratic(y)
    
    if (qx && qy) {
        out$Q <- purrr::map2(x$Q, y$Q, `+`)
    } 
    else if (qx || qy) {
        out$Q <- x$Q %||% y$Q
    }
    
    out$L <- x$L + y$L
    out$A <- out$A + y$A
    out$binary <- FALSE
    
    transformed_variable(out)
}
# var + constant
add_v_c <- function(x, c, call) {
    max_n <- max(length(x), length(c))
    x <- recycle_var(x, max_n)
    c <- recycle_const(c, max_n)
    
    x$A <- x$A + c
    x$binary <- FALSE
    
    transformed_variable(x)
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
    out <- add_v_c(minus_v(x), c, call)
    
    # If its (1-x) and x is binary, it stays binary
    if (x$binary && all(c == 1)) {
        out$binary <- TRUE
    }
    
    return(out)
}

# var * constant
multiply_v_c <- function(x, c, call) {
    max_n <- max(length(x), length(c))
    x <- recycle_var(x, max_n)
    c <- recycle_const(c, max_n)
    
    if (is_quadratic(x)) {
        x$Q <- q_list_multiply(x$Q, c)
    }
    
    x$L <- horizontal_multiply(x$L, c)
    x$A <- x$A * c
    x$binary <- FALSE
    
    transformed_variable(x)
}
# var * var
multiply_v_v <- function(x, y, call) {
    if (is_quadratic(x) || is_quadratic(y)) {
        cli_abort(
            "Non-quadratic operation", 
            class = "lpsugar_error_non_quadratic_operation",
            call = call
        )
    }
    
    max_n <- max(length(x), length(y))
    x <- recycle_var(x, max_n)
    y <- recycle_var(y, max_n)
    m <- ncol(x$L)
    out <- x
    
    out$Q <- lapply(seq_len(max_n), function(i) {
        xi <- x$L[i, ] |> c()
        yi <- y$L[i, ] |> c()
        qi <- outer(xi, yi) + outer(yi, xi)
        
        rownames(qi) <- colnames(qi) <- colnames(x$L)
        robust_index(qi)
    })
    
    out$L <-
        horizontal_multiply(x$L, y$A) +
        horizontal_multiply(y$L, x$A)
    
    out$A <- x$A * y$A
    transformed_variable(out)
}

# var / constant
divide_v_c <- function(x, c, call) {
    multiply_v_c(x, 1/c, call = call)
}
# any / var
divide_a_v <- function(x, y, call) {
    cli_abort(
        "Cannot divide by a variable in a linear problem.", 
        class = "lpsugar_error_unsupported_operation",
        call = call
    )
}

# var ^ constant
power_v_c <- function(x, c, call) {
    if (is_quadratic(x)) {
        cli_abort(
            "Non-quadratic operation", 
            class = "lpsugar_error_non_quadratic_operation",
            call = call
        )
    }
    
    max_n <- max(length(x), length(c))
    x <- recycle_var(x, max_n)
    c <- recycle_const(c, max_n)
    
    if (!all(c %in% 0:2)) {
        cli_abort(
            "Exponent must be 0, 1 or 2", 
            class = "lpsugar_error_non_quadratic_operation",
            call = call
        )
    }
    
    if (any(c == 2)) {
        i2 <- (c == 2)
        xsquared <- multiply_v_v(x, x, call)
        
        x$Q <- xsquared$Q
        x$Q[!i2] <- x$Q[!i2] |>
            lapply(\(q) q*0)
        
        x$L[i2, ] <- xsquared$L[i2, ]
        x$A[i2, ] <- xsquared$A[i2, ]
    }
    
    x$L[c == 0, ] <- 0
    x$A[c == 0, ] <- 1
    
    transformed_variable(x)
}

# Logic ------------------------

negate_v <- function(x, call) {
    if (!x$binary) {
        cli_abort(
            "Negation `!x` is only supported for binary variables.", 
            class = "lpsugar_error_negation_non_binary",
            call = call
        )
    }
    
    # 1 - x
    x$L <- -x$L
    x$A <- -x$A + 1
    
    transformed_variable(x)
}

# Matrix Operations -------------

#' @export
`%*%.lp_variable` <- function(x, y) {
    call <- rlang::call2("%*%", substitute(x), substitute(y))
    xv <- is_lp_variable(x)
    yv <- is_lp_variable(y)
    
    if (is_quadratic(x) || is_quadratic(y)) {
        cli_abort(
            "Non-quadratic operation", 
            class = "lpsugar_error_non_quadratic_operation",
            call = call
        )
    }
    
    if (xv && yv) {
        matrix_multiply_v_v(x, y, call = call)
    } 
    else if (xv) {
        matrix_multiply_v_c(x, y, call = call)
    } 
    else {
        t(matrix_multiply_v_c(t(y), t(x), call = call))
    }
}

# var %*% mat
matrix_multiply_v_c <- function(x, y, call) {
    if (ndim(x) > 2L) {
        cli_abort(
            "Variable has ({ndim(x)}) dimensions.",
            class = "lpsugar_error_not_2d"
        )
    } 
    else if (ndim(x) == 1L) {
        x$ind <- matrix(x$ind, ncol = 1L)
    }
    
    ptype <- rlang::try_fetch(x$ind %*% y, error = identity)
    
    if (rlang::is_error(ptype)) {
        cli_abort(ptype$message, call = call)
    }
    
    
    if (!is.matrix(y)) {
        y <- matrix(y, ncol = 1L)
    }
    
    out <- x
    out$ind <- ptype
    out$ind[] <- seq_along(out$ind)
    
    if (is_quadratic(x)) {
        # TODO
        cli_abort("`%*%` not yet implemented for quadratic variables.", call = call)
    }
    
    out$L <- out$L[integer(), , drop = TRUE]
    out$A <- out$A[integer(), , drop = TRUE]
    
    for (j in 1:ncol(y)) for (i in 1:nrow(x)) {
        z <- sum(x[i, ] * y[, j])
        out$L <- rbind(out$L, z$L)
        out$A <- rbind(out$A, z$A)
    }
    
    out$L <- robust_index(out$L)
    out$A <- robust_index(out$A)
    
    transformed_variable(out)
}

# var %*% var
matrix_multiply_v_v <- function(x, y, call) {
    x$ind <- drop(x$ind)
    y$ind <- drop(y$ind)
    
    ndx <- ndim(x$ind)
    ndy <- ndim(y$ind)
    
    if (ndx > 2L) {
        cli_abort(
            "Left-hand-side has {ndx} dimensions.",
            class = "lpsugar_error_not_2d"
        )
    } 
    else if (ndx == 1L) {
        x$ind <- matrix(x$ind, ncol = 1L)
    }
    
    if (ndy > 2L) {
        cli_abort(
            "Right-hand-side has {ndy} dimensions.",
            class = "lpsugar_error_not_2d"
        )
    } 
    else if (ndy == 1L) {
        y$ind <- matrix(y$ind, ncol = 1L)
    }
    
    ptype <- rlang::try_fetch(x$ind %*% y$ind, error = identity)
    
    if (rlang::is_error(ptype)) {
        cli_abort(ptype$message, call = call)
    }
    
    out <- x
    out$ind <- ptype
    out$ind[] <- seq_along(out$ind)
    
    out$Q <- list()
    out$L <- out$L[integer(), , drop = TRUE]
    out$A <- out$A[integer(), , drop = TRUE]
    
    for (j in 1:ncol(y)) for (i in 1:nrow(x)) {
        z <- sum(x[i, ] * y[, j])
        out$Q <- c(out$Q, z$Q)
        out$L <- rbind(out$L, z$L)
        out$A <- rbind(out$A, z$A)
    }
    
    out$L <- robust_index(out$L)
    out$A <- robust_index(out$A)
    
    transformed_variable(out)
}

# Methods -----------------------

#' @export
diff.lp_variable <- function(x, lag = 1L, differences = 1L, ...) {
    stopifnot(
        rlang::is_integerish(lag, n = 1, finite = TRUE),
        rlang::is_integerish(differences, n = 1, finite = TRUE)
    )
    if (lag < 1L || differences < 1L) {
        cli_abort(
            "`lag` and `differences` must be integers >= 1",
            class = "lpsugar_error_bad_lag_or_differences"
        )
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
    if (op == "<") {
        op <- "<="
    } 
    else if (op == ">") {
        op <- ">="
    }
    
    var <- x - y
    
    if (is_quadratic(var)) {
        Q <- lapply(var$Q, function(q) {
            if (any(q != 0)) {
                slam::as.simple_triplet_matrix(q)
            } 
            else {
                NULL
            }
        })
    } 
    else {
        Q <- rep(list(NULL), length(var))
    }
    
    L <- slam::as.simple_triplet_matrix(var$L)
    rhs <- -var$A
    
    dir <- rep(op, length(rhs))
    call <- rep(format1(call), length(rhs))
    name <- character(length(rhs))
    
    list(Q = Q, L = L, dir = dir, rhs = rhs, name = name, call = call) |>
        structure(class = "lp_constraint")
}

# Utils ----------------------

# Functions to apply vectorized operations on lp_variables

# Multiplies each row for a constant
# x[i, ] <- x[i, ] * c[i]
horizontal_multiply <- function(x, c) {
    stopifnot(length(c) == nrow(x) || length(c) == 1L)
    c <- array(c, dim = dim(x))
    x*c
}

# Multiplies each element of a list for a constant
# q[[i]] <- q[[i]] * c[i]
# meant for updating the quadratic `Q` field
q_list_multiply <- function(q, c) {
    stopifnot(length(q) == length(c))
    purrr::map2(q, c, `*`)
}
