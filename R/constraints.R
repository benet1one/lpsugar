
#' Add constraints to an [lp_problem()]
#'
#' Restrict the variables in an [lp_problem()] with linear constraints.
#'
#' @param .problem An [lp_problem()].
#' @param ... One or more linear constraints. Can be named. They must:
#' - Contain one or more variables defined with [lp_variable()]
#' - Contain a comparison operator ( `< / <= / == / => / >` )
#'
#' @returns The `.problem` with added `$constraints`. (Note: previous constraints are not
#' overritten).
#'
#' Constraints can be represented as `lhs * vars <dir> rhs`.
#'
#' The `$constraints` field has the following subfields:
#' - `$lhs` : Matrix where each row is a constraint, each column is a variable,
#' and the values represent coefficients.
#' - `$dir` : Character vector with elements `"<="`, `"=="`, or `">="`,
#' the direction of each constraint.
#' - `$rhs` : Numeric column vector representing the right hand side of each constraint.
#' - `$name` : Character vector with the names of the constraints, if `...` is named,
#' or `""` for unnamed constraints.
#' - `$call` : Expression that defined each constraint.
#'
#' @export
#'
#' @example inst/examples/example_constraint.R
lp_constraint <- function(.problem, ...) {
    check_problem(.problem)
    data <- data_mask(.problem)
    quos <- rlang::enquos(...)
    nams <- rlang::names2(quos)
    varnames <- c(names(.problem$variables), names(.problem$aliases))

    cons <- list()

    for (i in seq_along(quos)) {
        cons[[i]] <- lp_constraint_internal(
            quosure = quos[[i]],
            name = nams[i],
            data = data,
            varnames = varnames
        )
    }

    cons <- do.call(rbind.lp_constraint, cons)

    if (length(.problem$constraints) == 0L) {
        .problem$constraints <- cons
    } else {
        .problem$constraints <- rbind.lp_constraint(.problem$constraints, cons)
    }

    return(.problem)
}

lp_constraint_internal <- function(quosure, name, data, varnames) {
    expr <- rlang::quo_get_expr(quosure)
    vars <- all.vars(expr)

    if (!any(vars %in% varnames)) {
        abort("Constraint does not contain any variables.", call = expr)
    }

    con <- for_split(quosure, evaluate = TRUE, data = data)
    non_constraint_error <- c(
        "Expression did not evaluate to a constraint.",
        ">" = "Did you forget the comparison operator? `<=/==/>=`"
    )

    if (is_lp_constraint(con)) {
        rownames(con$lhs) <- rep(name, nrow(con$lhs))
        con$name[] <- name

    } else if (is_for_split(con)) {
        fs <- flatten(con)

        for (k in seq_along(fs)) {
            c <- fs[[k]]
            ind <- names(fs)[k]

            if (!is_lp_constraint(c)) {
                rlang::abort(non_constraint_error, call = expr)
            }

            rownames(c$lhs) <- rep(paste0(name, ind), nrow(c$lhs))
            fs[[k]] <- c
        }

        con <- do.call(rbind.lp_constraint, fs)
        con$name[] <- name

    } else {
        rlang::abort(non_constraint_error, call = expr)
    }

    expr_str <- format(expr)

    if (length(expr_str) == 1L) {
        con$call[] <- expr_str
    } else {
        is_bracket <- expr_str[1] |> endsWith("{")
        con$call[] <- paste(expr_str[1], "...", if (is_bracket) "}")
    }

    return(con)
}

#' Delete constraints
#'
#' Remove named constraints from an [lp_problem()].
#'
#' @param .problem An [lp_problem()].
#' @param names Characted vector with the names of constraints to be deleted.
#' It is not possible to delete unnamed constraints, so make sure to name them
#' if you plan to delete them later.
#'
#' @export
#' @examples
lp_delete_constraint <- function(.problem, names) {
    check_problem(.problem)
    stopifnot(is.character(names))

    if (any(names == "") || any(names == "<unnamed>")) {
        warn("Cannot delete unnamed constraints.")
        names <- names[names != "" & names != "<unnamed>"]
    }

    not_defined <- which(!(names %in% .problem$constraint$name))

    if (length(not_defined) > 0L) {
        not_defined <- names[not_defined] |>
            utils::head(6) |>
            dQuote(q = FALSE) |>
            paste(collapse = ", ")

        warn("The following constraints are not defined: \n{not_defined}")
    }

    to_delete <- .problem$constraints$name %in% names
    .problem$constraints <- .problem$constraints[!to_delete]
    return(.problem)
}

# Alias ----------------------------------

#' @rdname lp_constraint
#' @export
lp_con <- lp_constraint
#' @rdname lp_constraint
#' @export
lp_subject_to <- lp_constraint


# List of Constraints --------------------

update_constraints <- function(.problem) {
    if (length(.problem$constraints) == 0L) {
        return(.problem)
    }

    lhs <- .problem$constraints$lhs
    to_bind <- matrix(0, nrow = nrow(lhs), ncol = .problem$.nvar - ncol(lhs))
    lhs <- cbind(lhs, to_bind)
    colnames(lhs) <- .problem$.varnames

    .problem$constraints$lhs <- lhs
    .problem
}

# Methods ----------------------

#' @export
rbind.lp_constraint <- function(..., deparse.level = 1) {
    dots <- rlang::dots_list(...)

    for (d in dots) if (!is_lp_constraint(d)) {
        abort("Cannot `rbind.lp_constraint` with other classes.")
    }

    out <- purrr::list_transpose(dots, simplify = FALSE)

    out$lhs <- do.call(what = rbind, out$lhs) |> robust_index()
    out$rhs <- do.call(what = rbind, out$rhs) |> robust_index()
    out$dir <- unlist(out$dir)
    out$call <- unlist(out$call)
    out$name <- unlist(out$name)

    structure(out, class = "lp_constraint")
}

#' @export
as.matrix.lp_constraint <- function(x, ...) {
    cbind(x$lhs, dir = x$dir, rhs = x$rhs)
}
#' @export
as.array.lp_constraint <- function(x, ...) {
    as.matrix.lp_constraint(x)
}

#' @export
length.lp_constraint <- function(x) {
    length(x$dir)
}
#' @export
dim.lp_constraint <- function(x) {
    c(length(x$dir), NA)
}
#' @export
`[.lp_constraint` <- function(x, ...) {
    dots <- rlang::dots_list(..., .preserve_empty = TRUE, .ignore_empty = "none")

    wrong_index <-
        length(dots) == 0L ||
        rlang::is_missing(dots[[1L]]) ||
        length(dots) > 2L ||
        (length(dots) == 2L && !rlang::is_missing(dots[[2L]]))

    if (wrong_index) {
        xname <- rlang::enexpr(x) |> format()
        abort("Index constraints with `con[i]` or `con[i, ]`")
    }

    i <- dots[[1L]]

    if (is.character(i)) {
        i <- x$name %in% i
    }

    x$lhs <- x$lhs[i, ]
    x$rhs <- x$rhs[i, ]
    x$dir <- x$dir[i]
    x$call <- x$call[i]
    x$name <- x$name[i]

    return(x)
}

#' @export
print.lp_constraint <- function(x, compact = FALSE, ...) {
    if (!compact) {
        m <- as.matrix.lp_constraint(x)
    }

    for (call in unique(x$call)) {
        nam <- x$name[x$call == call][1L]

        if (nam == "") {
            nam <- "<unnamed>"
        }

        cat("\n", nam, "|", call)

        if (!compact) {
            cat("\n\n")
            mc <- m[x$call == call, ]
            print(mc, quote = FALSE)
            cat("\n")
        }
    }

    invisible(x)
}
