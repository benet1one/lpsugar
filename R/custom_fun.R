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
