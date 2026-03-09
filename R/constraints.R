
lp_constraint <- function(.problem, ...) {
    data <- data_mask(.problem)
    quos <- rlang::enquos(...)
    cons <- purrr::map2(quos, rlang::names2(quos), function(q, name) {
        lp_constraint_internal(quosure = q, data = data, name = name)
    })

    cons <- do.call(rbind.lp_constraint, cons)

    if (length(.problem$constraints) == 0L) {
        .problem$constraints <- cons
    } else {
        .problem$constraints <- rbind.lp_constraint(.problem$constraints, cons)
    }

    return(.problem)
}

lp_constraint_internal <- function(quosure, data, name) {
    con <- for_split(quosure, evaluate = TRUE, data = data)

    expr <- rlang::quo_get_expr(quosure)
    non_constraint_error <- glue::glue(
        "Expression did not evaluate to a constraint.\n",
        "Did you forget the comparison operator? `<=/==/>=`"
    )

    if (is_lp_constraint(con)) {
        rownames(con$lhs) <- rep(name, nrow(con$lhs))
        con$name[] <- name
        return(con)

    } else if (is_for_split(con)) {
        fs <- flatten(con)

        for (k in seq_along(fs)) {
            c <- fs[[k]]
            ind <- names(fs)[k]

            if (!is_lp_constraint(c)) {
                abort(non_constraint_error, call = expr)
            }

            rownames(c$lhs) <- rep(paste0(name, ind), nrow(c$lhs))
            fs[[k]] <- c
        }

        con <- do.call(rbind.lp_constraint, fs)
        con$name[] <- name

        expr_str <- format(expr)

        if (length(expr_str) == 1L) {
            con$call[] <- format(expr)
        }

        return(con)
    }

    abort(non_constraint_error, call = expr)
}

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
        abort("cannot `rbind.lp_constraint` with other classes.")
    }

    out <- purrr::list_transpose(dots)
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
`[.lp_constraint` <- function(x, ...) {
    dots <- rlang::dots_list(..., .preserve_empty = TRUE, .ignore_empty = "none")

    wrong_index <-
        length(dots) == 0L ||
        rlang::is_missing(dots[[1L]]) ||
        length(dots) > 2L ||
        (length(dots) == 2L && !rlang::is_missing(dots[[2L]]))

    if (wrong_index) {
        xname <- rlang::enexpr(x) |> format()
        abort("index constraints with `con[i]` or `con[i, ]`")
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
        }
    }

    invisible(x)
}
