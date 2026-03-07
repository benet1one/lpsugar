
# List of Constraints --------------------

update_constraints <- function(.problem) {
    # TODO
    .problem
}

# Constraint Groups ----------------------

#' @export
as.matrix.lp_constraint <- function(x, ...) {
    cbind(x$lhs, dir = x$dir, rhs = x$rhs) |> structure(call = x$call)
}
#' @export
as.array.lp_constraint <- function(x, ...) {
    as.matrix.lp_constraint(x)
}
#' @export
`[.lp_constraint` <- function(x, ...) {
    dots <- rlang::dots_list(...)

    wrong_index <-
        length(dots) > 2L ||
        (length(dots) == 2L && rlang::is_missing(dots[[2L]]))

    if (wrong_index) {
        xname <- rlang::enexpr(x) |> format()
        abort("index constraints with `{xname}[i]` or `{xname}[i, ]`")
    }

    x[...]
}

#' @export
print.lp_constraint <- function(x, ...) {
    cat("Call:", format(x$call), "\n")
    print(as.matrix.lp_constraint(x))
    invisible(x)
}
