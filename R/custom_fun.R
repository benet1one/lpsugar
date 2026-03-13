
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
weighted.mean.lp_variable <- function(x, w, ..., na.rm = FALSE) {
    warn_changed_args(na.rm = FALSE)
    rlang::check_dots_empty()
    stats:::weighted.mean.default(x, w)
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

    e$apply <- function(X, MARGIN, FUN, ..., simplify = TRUE) {
        warn_changed_args(simplify = TRUE)
        apply_v(X, MARGIN, FUN, ...)
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

    # TODO
    # Get prototype dim by applying to ind?
    # Concatenate variables? What happens to ind then?
}
