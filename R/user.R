
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
    n <- lengths(dots)[1]
    m <- lengths(dots)[2]

    if (ndim(.x) == 1L) {
        if (length(.x) != 1L && length(.x) != n*m) {
            abort("`.x` is length ({length(.x)}) when it should be length ({n*m} = {n} x {m})",
                  call = parent.frame())
        }

        return(matrix(
            .x, nrow = n, ncol = m,
            byrow = byrow, dimnames = dots
        ))

    } else if (ndim(.x) == 2L) {
        if (any(dim(.x) != lengths(dots))) {
            msg <- glue::glue(
                "`.x` has dimensions ({nrow(.x)} x {ncol(.x)}), ",
                "while `...` have dimensions ({n} x {m})."
            )
            abort(msg, call = parent.frame())
        }

        warn_changed_args(byrow = TRUE)
        dimnames(.x) <- dots
        return(.x)

    } else {
        abort("`.x` has ({ndim(.x)}) dimensions. Only vectors and matrices are supported.",
              call = parent.frame())
    }
}


#' Compute a Summary of a Solution or Point
#'
#' Check feasibility, constraint saturation, calculate objective value and aliases.
#'
#' @param problem An [lp_problem()].
#' @param solution One of:
#' - Named list of variables with their respective values.
#' If a variable is missing, it is set to `max(0, lower)`.
#' - An `lp_solution` object as returned by [lp_solve()].
#' - A vector containing the values of each variable, one after another.
#' @param tol Tolerance to use for checking constraint and bound feasibility
#'
#' @returns A named list with the computed statistics.
#' @export
#'
#' @examples
solution_summary <- function(problem, solution, tol = 2e-6) {
    solution <- solution_to_vec(problem, solution)

    list(
        constraints = constraint_summary(problem, solution, tol = tol)
    )
}

solution_to_vec <- function(problem, solution, call = parent.frame()) {
    if (is_lp_solution(solution)) {
        return(solution$variables_vec)

    } else if (is.atomic(solution)) {
        if (length(solution) != problem$.nvar) {
            n <- problem$.nvar
            m <- length(solution)
            abort("`problem` has ({n}) variables but `solution` is length ({m}).",
                  call = call)
        }

        num <- as.numeric(solution)
        names(num) <- problem$.varnames
        return(num)

    } else if (is.list(solution)) {
        solution_vec <- numeric(problem$.nvar)
        names(solution_vec) <- problem$.varnames

        for (x in problem$variables) {
            xs <- solution[[x$name]]
            lower <- rep_len(x$lower, length(x))
            if (x$integer) lower <- ceiling(lower)
            default <- pmax(0, lower)

            if (is.null(xs)) {
                xs <- default
            } else {
                xs <- as.numeric(xs)
                xs[is.na(xs)] <- default[is.na(xs)]
            }

            if (length(xs) != length(x)) {
                n <- length(x)
                m <- length(xs)
                abort("Variable '{x$name}' should be length ({n}) but is length ({m}) in `solution`.",
                      call = call)
            }

            if (x$integer && !rlang::is_integerish(xs)) {
                warn("'{x$name}' should be integer.", call = call)
            }

            solution_vec[x$ind] <- xs
        }

        return(solution_vec)

    } else {
        abort("Unsupported type for `solution`.", call = call)
    }
}

#' @rdname solution_summary
#' @export
constraint_summary <- function(problem, solution, tol = 2e-6) {
    solution <- solution_to_vec(problem, solution)
    con <- problem$constraints
    lhs <- con$lhs %*% solution
    lhs <- lhs[, 1]
    dir <- con$dir
    rhs <- con$rhs[, 1]
    diff <- rhs - lhs

    less_than <- lhs <= rhs + tol
    greater_than <- lhs >= rhs - tol
    equal_to <- less_than & greater_than

    satisfied <- logical(length(dir))
    satisfied[dir == "<="] <- less_than[dir == "<="]
    satisfied[dir == ">="] <- greater_than[dir == ">="]
    satisfied[dir == "=="] <- equal_to[dir == "=="]

    feasible <- all(satisfied)
    saturated <- diff > -tol  &  diff < +tol
    saturated[!satisfied] <- NA

    df <- data.frame(
        name = con$name,
        fullname = rownames(con),
        lhs = lhs,
        dir = dir,
        rhs = rhs,
        diff = diff,
        satisfied = satisfied,
        saturated = saturated
    )

    list(
        data = df,
        feasible = feasible
    )
}
