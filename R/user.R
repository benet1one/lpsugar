
#' Set Dimensions and Names for Parameters
#'
#' Return a named vector or matrix.
#'
#' @param .x Vector or matrix of any type.
#' @param ... Dimnames. If you want to return a vector, it needs one element;
#' if you want to return a matrix it needs two elements.
#' @param byrow Boolean, true by default. Only used if `.x` is a vector and `...` has two elements.
#'
#' @returns A named vector or matrix.
#' @export
#'
#' @examples
#' my_set <- letters[1:3]
#' my_parameter <- c(2, 6, 3) |> parameter(my_set)
#' my_parameter
#'
#' rows <- letters[1:2]
#' cols <- LETTERS[1:3]
#' my_matrix <- c(
#'     1, 2, 3,
#'     4, 5, 6
#' ) |> parameter(rows, cols)
#' my_matrix
#'
#' # Also works if .x is already a matrix.
#' mat <- matrix(1:6, nrow = 3, ncol = 2)
#' mat |> parameter(r = letters[1:3], c = LETTERS[1:2])
parameter <- function(.x, ..., byrow = TRUE) {
    dots <- rlang::dots_list(..., .named = TRUE, .ignore_empty = "none")

    if (length(dots) == 1L) {
        warn_changed_args(byrow = TRUE)
        parameter_vector(.x, dots)

    } else if (length(dots) == 2L) {
        parameter_matrix(.x, dots, byrow = byrow)

    } else if (length(dots) == 0L) {
        abort("`...` cannot be empty.")

    } else {
        abort("Only vectors and matrices are supported.")
    }
}

parameter_vector <- function(.x, dots) {
    nd <- sum(dim2(.x) > 1L)

    if (nd > 1L) {
        abort("`.x` has two or more dimensions but only one element in `...`",
              call = parent.frame())
    }

    n <- length(dots[[1]])

    if (length(.x) > 1L && length(.x) != n) {
        abort("`.x` is length ({length(.x)}) and `{names(dots)}` is length ({n}).")
    }

    array(.x, dim = length(dots[[1]]), dimnames = dots)
}

parameter_matrix <- function(.x, dots, byrow = TRUE) {
    if (ndim(.x) == 1L) {
        return(matrix(
            .x,
            nrow = length(dots[[1]]),
            ncol = length(dots[[2]]),
            byrow = byrow,
            dimnames = dots
        ))

    } else if (ndim(.x) == 2L) {
        warn_changed_args(byrow = TRUE)
        dimnames(.x) <- dots
        return(.x)

    } else {
        abort("`.x` has ({ndim(.x)}) dimensions. Only vectors and matrices are supported.",
              call = parent.frame())
    }
}
