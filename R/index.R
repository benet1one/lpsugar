
# check out-of-bounds indices and makes drop = FALSE by default
robust_index <- function(x) {
    structure(x, class = c("robust_index", class(x)))
}

#' @export
print.robust_index <- function(x, ...) {
    print(unclass(x))
    cat("with class 'robust_index' from package 'lpsugar'\n")
    invisible(x)
}

#' @export
`[.robust_index` <- function(x, ..., drop = FALSE) {
    if (...length() == 1L) {
        check_index_valid_vector(x, index = c(...))

    } else if (is.array(x)) {
        d <- dim(x)

        if (...length() != length(d)) {
            abort("Incorrect number of dimensions.")
        }

        dots <- rlang::dots_list(..., .ignore_empty = "none", .preserve_empty = TRUE)

        for (margin in 1:length(d)) {
            dm <- dots[[margin]]

            if (rlang::is_missing(dm)) {
                next
            }

            im <- eval(dm, envir = parent.frame())

            check_index_valid_array(
                x,
                margin = margin,
                index = im
            )
        }

    } else if (is.vector(x)) {
        if (...length() != 1L) {
            abort("Incorrect number of dimensions.")
        }

        check_index_valid_vector(x, index = im)
    }

    NextMethod(drop = drop) |> robust_index()
}

check_index_valid_array <- function(x, margin, index, call = parent.frame()) {
    if (is.logical(index)) {
        if (anyNA(index)) {
            abort("Subscript is NA", call = call)
        }

        if (length(index) != dim(x)[margin]) {
            abort("Subscript length mismatch in dimension {margin}", call = call)
        }

    } else if (is.numeric(index)) {
        zero <- match(TRUE, index > -1L & index < 1L)

        if (!is.na(zero)) {
            abort("Invalid subscript ({index[zero]}) in dimension {margin}.", call = call)
        }

        cap <- dim(x)[margin]
        oob <- match(TRUE, index <= -cap - 1L | index >= cap + 1L)

        if (!is.na(oob)) {
            abort("Subscript ({index[oob]}) out of bounds in dimension {margin}.", call = call)
        }

    } else if (is.character(index) || is.factor(index)) {
        index <- as.character(index)
        nams <- dimnames(x)[[margin]]

        if (is.null(nams)) {
            abort("Dimension {margin} is unnamed.", call = call)
        }

        missing_name <- match(TRUE, !is.element(index, nams))

        if (!is.na(missing_name)) {
            abort("Invalid subscript '{index[missing_name]}' in dimension {margin}.", call = call)
        }

    } else {
        abort("Invalid subscript of class `{class(index)}`.", call = call)
    }
}
check_index_valid_vector <- function(x, index, call = parent.frame()) {
    if (is.logical(index)) {
        if (anyNA(index)) {
            abort("Subscript is NA", call = call)
        }

        if (length(index) != length(x)) {
            abort("Subscript length mismatch", call = call)
        }

    } else if (is.numeric(index)) {
        zero <- match(TRUE, index > -1L & index < 1L)

        if (!is.na(zero)) {
            abort("Invalid subscript ({index[zero]}).", call = call)
        }

        cap <- length(x)
        oob <- match(TRUE, index <= -cap - 1L | index >= cap + 1L)

        if (!is.na(oob)) {
            abort("Subscript ({index[oob]}) out of bounds.", call = call)
        }

    } else if (is.character(index) || is.factor(index)) {
        index <- as.character(index)
        nams <- names(x)

        if (is.null(nams)) {
            abort("Vector is unnamed.", call = call)
        }

        missing_name <- match(TRUE, !is.element(index, nams))

        if (!is.na(missing_name)) {
            abort("Invalid subscript '{index[missing_name]}'.", call = call)
        }

    } else {
        abort("Invalid subscript of class `{class(index)}`.", call = call)
    }
}
