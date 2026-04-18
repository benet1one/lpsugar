
# Sum For ---------------------------------------

#' Index Based Summation
#'
#' Sum over one or more indexing variables.
#'
#' @param ... The first argument(s) must be named: the name represents the indexing variable,
#' and the values represent the sequence over which to sum.
#'
#' The last argument is the expression of the sum.
#'
#' @details
#' The syntax is similar to the one used in math. For instance,
#' \eqn{ \sum_{i=1}^n \sum_{j=1}^m {x_{ij} * c_j} }
#' would be written as
#' `sum_over(i = 1:n, j = 1:n, x[i,j] * c[j])`.
#'
#' @returns The value of the sum.
#' @export
#'
#' @examples
#' cost <- c(5, 2, 7)
#' p <- lp_problem() |>
#'   lp_variable(x[1:2, 1:3]) |>
#'   lp_minimize(sum_over(i = 1:2, j = 1:3, x[i, j] * cost[j]))
#' p$objective
sum_over <- function(...) {
    dots <- rlang::enquos(..., .homonyms = "error")
    nams <- rlang::names2(dots)
    n <- length(dots)

    if (nams[n] != "") {
        abort("Last element in `...` should be an unnamed expression to sum.")
    }
    if (any(nams[-n] == "")) {
        # TODO Fix the grammar in this error message
        abort("The first elements in `...` should be the named indices to sum across.")
    }

    env <- rlang::get_env(dots[[n]])
    indices <- dots[-n] |> lapply(rlang::eval_tidy)
    grid <- do.call(expand.grid, indices)
    s <- 0

    if (nrow(grid) > 0) for (i in 1:nrow(grid)) {
        sub_frame <- grid[i, , drop = FALSE] |>
            as.list()
        expr <- dots[[n]] |>
            rlang::get_expr() |>
            methods::substituteDirect(sub_frame)

        r <- rlang::eval_tidy(expr = expr, env = env)
        s <- s + r
    }

    sum(s)
}


# Methods ---------------------------------------

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
mean.lp_variable <- function(x, ...) {
    sum(x) / length(x)
}

#' @export
Math.lp_variable <- function(x, ...) {
    fun <- .Generic
    call <- paste0(fun, "(", format(substitute(x)), ")") |>
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


# Custom Function Data Mask ---------------------

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

    # If weighted.mean is a method, it only covers the case where x is a variable
    # and not when w is a variable (which should throw an error because it's not linear)
    e$weighted.mean <- function(x, w, ..., na.rm = FALSE) {
        if (!missing(w) && is_lp_variable(w)) {
            abort("Weights `w` cannot be a variable")
        }
        if (is_lp_variable(x)) {
            rlang::check_dots_empty()
            warn_changed_args(na.rm = FALSE)
            na.rm <- FALSE
        }

        return(stats::weighted.mean(x, w, ..., na.rm))
    }

    e$ifelse <- function(test, yes, no) {
        if (is_lp_constraint(test)) {
            abort("The `test` condition must be a binary variable, not an equality or inequality.")
        }
        if (is_lp_constraint(yes) || is_lp_constraint(no)) {
            abort("`yes` and `no` cannot be constraints.")
        }
        if (is_lp_variable(test)) {
            ifelse_v(test, yes, no)
        } else if (is_lp_variable(yes) || is_lp_variable(no)) {
            ifelse_l(test, yes, no)
        } else {
            base::ifelse(test, yes, no)
        }
    }

    e$apply <- function(X, MARGIN, FUN, ..., simplify = TRUE) {
        if (!is_lp_variable(X)) {
            return(base::apply(X, MARGIN, FUN, ..., simplify))
        }

        warn_changed_args(simplify = TRUE)
        apply_v(X, MARGIN, FUN, ...)
    }

    e$rowSums <- function(x, na.rm = FALSE, dims = 1L) {
        if (!is_lp_variable(x)) {
            return(base::rowSums(x, na.rm, dims = dims))
        }

        warn_changed_args(na.rm = FALSE, dims = 1L)
        apply_v(x, 1L, sum)
    }
    e$colSums <- function(x, na.rm = FALSE, dims = 1L) {
        if (!is_lp_variable(x)) {
            return(base::colSums(x, na.rm, dims = dims))
        }

        warn_changed_args(na.rm = FALSE, dims = 1L)
        apply_v(x, 2L, sum)
    }
    e$rowMeans <- function(x, na.rm = FALSE, dims = 1L) {
        if (!is_lp_variable(x)) {
            return(base::rowMeans(x, na.rm, dims = dims))
        }

        warn_changed_args(na.rm = FALSE, dims = 1L)
        apply_v(x, 1L, mean)
    }
    e$colMeans <- function(x, na.rm = FALSE, dims = 1L) {
        if (!is_lp_variable(x)) {
            return(base::colMeans(x, na.rm, dims = dims))
        }

        warn_changed_args(na.rm = FALSE, dims = 1L)
        apply_v(x, 2L, mean)
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

apply_v <- function(x, margin, fun, ...) {
    ind <- x$ind
    ind[] <- 1:length(ind)
    ind_list <- apply(ind, margin, identity, simplify = FALSE)

    i1 <- ind_list[[1]]
    ptype <- fun(x[i1])

    if (!is_lp_variable(ptype)) {
        return(vapply(ind_list, \(i) fun(x[i]), ptype))
    }

    y <- ptype

    for (i in ind_list[-1]) {
        y_i <- fun(x[i])
        y$coef <- rbind(y$coef, y_i$coef)
        y$add <- rbind(y$add, y_i$add)
    }

    y$coef <- y$coef |> robust_index()
    y$add <- y$add |> robust_index()

    y_ind <- apply(ind, margin, fun, ..., simplify = FALSE) |>
        simplify2array(higher = TRUE, except = 0)

    if (is.array(y_ind)) {
        y_ind[] <- 1:length(y_ind)
        y$ind <- y_ind |> robust_index()
    } else {
        y$ind <- 1:nrow(y$coef)
    }

    return(y)
}

ifelse_v <- function(test, yes, no) {
    if (!test$binary) {
        abort("`test` must be a binary variable.")
    }
    if (is_lp_variable(yes) || is_lp_variable(no)) {
        abort("If `test` is a variable, `yes` and `no` must be numbers, not variables.")
    }

    no + test * (yes - no)
}
ifelse_l <- function(test, yes, no) {
    if (is.atomic(test)) {
        if (typeof(test) != "logical")
            storage.mode(test) <- "logical"
        if (length(test) == 1 && is.null(attributes(test))) {
            if (is.na(test))
                return(NA)
            else if (test) {
                if (length(yes) == 1) {
                    yat <- attributes(yes)
                    if (is.null(yat) || (is.function(yes) && identical(names(yat),
                                                                       "srcref")))
                        return(yes)
                }
            }
            else if (length(no) == 1) {
                nat <- attributes(no)
                if (is.null(nat) || (is.function(no) && identical(names(nat),
                                                                  "srcref")))
                    return(no)
            }
        }
    }
    else test <- if (isS4(test))
        methods::as(test, "logical")
    else as.logical(test)


    len <- length(test)

    # One of them is a variable
    if (is_lp_variable(yes)) {
        yes_v <- recycle_var(yes, len)
        v <- yes_v
    }
    if (is_lp_variable(no)) {
        no_v <- recycle_var(no, len)
        v <- no_v
    }

    if (!is_lp_variable(yes)) {
        yes_v <- v
        yes_v$coef[] <- 0
        yes_v$add[] <- rep_len(yes, len)
    }
    if (!is_lp_variable(no)) {
        no_v <- v
        no_v$coef[] <- 0
        no_v$add[] <- rep_len(no, len)
    }

    out <- v
    out$add <- matrix(NA, nrow = len, ncol = 1L)
    out$coef <- matrix(NA, nrow = len, ncol = ncol(v$coef))
    colnames(out$coef) <- colnames(v$coef)

    ypos <- which(test)
    npos <- which(!test)

    if (length(ypos) > 0L) {
        out$coef[ypos, ] <- yes_v$coef[ypos, ]
        out$add[ypos, ] <- yes_v$add[ypos, ]
    }
    if (length(npos) > 0L) {
        out$coef[npos, ] <- no_v$coef[npos, ]
        out$add[npos, ] <- no_v$add[npos, ]
    }

    out
}
