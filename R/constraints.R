
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

        return(con)
    }

    abort(non_constraint_error, call = expr)
}

# List of Constraints --------------------

update_constraints <- function(.problem) {
    # TODO
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
    dots <- rlang::dots_list(...)

    wrong_index <-
        length(dots) > 2L ||
        (length(dots) == 2L && rlang::is_missing(dots[[2L]]))

    if (wrong_index) {
        xname <- rlang::enexpr(x) |> format()
        abort("index constraints with `{xname}[i]` or `{xname}[i, ]`")
    }

    # TODO
}

#' @export
print.lp_constraint <- function(x, ...) {
    m <- as.matrix.lp_constraint(x)

    for (call in unique(x$call)) {
        mc <- m[x$call == call, ]
        nam <- x$name[x$call == call][1L]
        cat("\n", nam, "|", call, "\n\n")
        print(mc, quote = FALSE)
    }

    invisible(x)
}
