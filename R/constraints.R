
#' Add Constraints to an [lp_problem()]
#'
#' Restrict the variables in an [lp_problem()] with linear or quadratic constraints.
#'
#' @param .problem An [lp_problem()].
#' @param ... One or more linear constraints. Can be named. They must:
#' - Contain one or more variables defined with [lp_variable()]
#' - Contain a comparison operator, such as `<=`,  `==` or `=>`.
#'
#' @returns The `.problem` with added `$constraints`. (Note: previous constraints are not
#' overritten).
#'
#' A constraint with `dir[i] = "<="` is represented as 
#' \eqn{\frac{1}{2} x'Q_{i}x + L_{i}x \le \text{rhs}_{i}}.
#'
#' The `$constraints` field has the following subfields:
#' - `$Q` : List of quadratic coefficient matrices:
#'   - `NULL` if the constraint is linear.
#'   - [slam::simple_triplet_matrix()] if constraint is quadratic.
#' - `$L` : [slam::simple_triplet_matrix()] of linear coefficients, 
#' where each row is a constraint and each is a variable.
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
    quos <- rlang::enquos(...)
    nams <- rlang::names2(quos)
    data <- data_mask(.problem)
    varnames <- c(
        names(.problem$variables), 
        names(.problem$aliases)
    )
    
    cons <- list()
    
    for (i in seq_along(quos)) {
        cons[[i]] <- lp_constraint_internal(
            quosure = quos[[i]],
            id = nams[i],
            data = data,
            varnames = varnames,
            problem = .problem
        )
    }
    
    .problem$constraints <- bind_cons(.problem$constraints, !!!cons)
    
    if (length(.problem$constraints) == 0L) {
        .problem$constraints <- empty_constraint(n = ncol(.problem))
    }
    
    return(.problem)
}

lp_constraint_internal <- function(quosure, id, data, varnames, problem) {
    expr <- rlang::quo_get_expr(quosure)
    vars <- all.vars(expr)
    
    if (!any(vars %in% varnames)) {
        cli_abort(
            "Constraint does not contain any variables.", 
            class = "lpsugar_error_no_constraint",
            call = expr
        )
    }
    
    cons <- eval_split_for(quosure, data = data)
    expr <- format1(quosure)
    ids <- rlang::names2(cons)
    
    indices <- ifelse(
        ids != "", 
        paste0(id, "[", ids, "]"),
        id
    )
    
    for (i in seq_along(cons)) {
        if (is.null(cons[[i]])) {
            next
        }
        
        if (inherits(cons[[i]], "nonlinear")) {
            nonlinear_constraint_form_error()
        }
        if (!is_lp_constraint(cons[[i]])) {
            msg <- c(
                "Expression did not evaluate to a constraint.",
                "x" = "Problematic constraint: '{indices[i]}'.",
                ">" = "Did you forget the comparison operator? `<=/==/>=`"
            )
            
            cli_abort(msg, call = quosure, class = "lpsugar_error_no_constraint")
        }
        
        info <- lpsugar_attributes(cons[[i]])
        info$id[] <- id
        info$expr[] <- expr
        info$index[] <- indices[i]
        lpsugar_attributes(cons[[i]]) <- info
        
        if (!is.null(cons[[i]]$L)) {
            rownames(cons[[i]]$L) <- info$index
        }
    }
    
    if (length(cons) != 1L) {
        bind_cons(!!!cons)
    }
    else {
        cons[[1]]
    }
}

#' Delete Constraints
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
#' p <- lp_problem() |>
#'     lp_variable(x) |>
#'     lp_constraint(
#'         c1 = x > 0,
#'         c2 = x < 10
#'     )
#'
#' some_condition <- TRUE
#' if (some_condition) {
#'     p <- p |> lp_delete_constraint("c1")
#' }
#'
#' print(p)
lp_delete_constraint <- function(.problem, ids) {
    check_problem(.problem)
    stopifnot(is.character(ids))
    info <- lpsugar_attributes(.problem$constraints)
    
    if (any(ids == "") || any(ids == "#unnamed_constraint")) {
        cli_abort(
            "Cannot delete unnamed constraints.",
            class = "lpsugar_error_delete_unnamed_constraints"
        )
    }
    
    undefined <- setdiff(ids, info$id)
    
    if (length(undefined) > 0L) {
        cli_warn(
            c("Cannot delete constraints that haven't been defined.",
              "x" = "Ignoring constraints: {.str {undefined}}"),
            class = "lpsugar_warning_delete_undefined_constraints"
        )
        
        ids <- intersect(ids, info$id)
    }
    
    to_delete <- info$id %in% ids
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


# Utils --------------------

new_constraint <- function(roi_constraint, call) {
    n <- length(roi_constraint$rhs)
    
    structure(
        roi_constraint,
        class = c("lp_constraint", class(roi_constraint)),
        lpsugar_attributes = list(
            id = character(n),
            index = character(n),
            expr = rep_len(format1(call), n)
        )
    )
}

empty_constraint <- function(n = 0) {
    roi_constraint <- ROI::NO_constraint(n)
    
    structure(
        roi_constraint,
        class = c("lp_empty_constraint", "lp_constraint", class(roi_constraint)),
        lpsugar_attributes = list(
            id = character(0),
            index = character(0),
            expr = character(0)
        )
    )
}

update_constraints <- function(.problem) {
    if (length(.problem$constraints) == 0L) {
        .problem$constraints <- empty_constraint(ncol(.problem))
    }
    
    varnames <- variable.names(.problem)
    q_ind <- which(lengths(.problem$constraints$Q) > 0L)
    
    for (i in q_ind) {
        .problem$constraints$Q[[i]]$nrow[] <- ncol(.problem)
        .problem$constraints$Q[[i]]$ncol[] <- ncol(.problem)
        .problem$constraints$Q[[i]]$dimnames <- list(varnames, varnames)
    }
    
    
    .problem$constraints$L$ncol[] <- ncol(.problem)
    colnames(.problem$constraints$L) <- varnames
    .problem$constraints$names <- varnames
    
    .problem
}

#' Define Multiple Constraints at Once
#'
#' Concatenate constraints.
#'
#' @param ... Constraints. See [lp_constraint()].
#'
#' @export
#' @example inst/examples/example_bind_cons.R
bind_cons <- function(...) {
    dots <- rlang::dots_list(...)
    dots <- dots[lengths(dots) > 0]
    dots <- purrr::keep(dots, function(d) {
        if (!is_lp_constraint(d)) {
            cli_abort(
                "`bind_cons()` can only bind <lp_constraint>, not <{class(d)[1]}>.",
                class = "lpsugar_error_bind_non_constraint"
            )
        }
        
        !is_empty_constraint(d)
    })
    
    if (length(dots) == 0L) {
        return(empty_constraint(0))
    }
    
    roi_binder <- get("rbind.constraint", pos = getNamespace("ROI"))
    out <- rlang::exec(roi_binder, !!!dots)
    class(out) <- c("lp_constraint", class(out))
    
    lpsugar_attributes(out) <- purrr::map(dots, lpsugar_attributes) |> 
        purrr::list_transpose(simplify = FALSE) |> 
        purrr::map(\(x) unlist(x, use.names = FALSE))

    return(out)
}

roi_constraint_class <- function(con) {
    out <- grepv(class(con), pattern = "^[A-Z]_constraint$")[1]
    
    if (length(out) == 0) {
        class(con)[1]
    } else {
        out
    }
}

# Methods ----------------------

#' @export
rbind.lp_constraint <- function(..., deparse.level = 1) {
    warn_changed_args(deparse.level = 1)
    bind_cons(...)
}

#' @export
as.matrix.lp_constraint <- function(x, ...) {
    acceptable <- any(
        inherits(x, "L_constraint"),
        inherits(x, "Q_constraint") && !is_quadratic(x)
    )
    
    if (!acceptable) {
        cli_abort(
            c("Can only convert linear constraints into matrices.",
              "x" = "`x` is <{roi_constraint_class(x)}>"),
            class = "lpsugar_error_as_matrix_constraint_not_linear"
        )
    }
    
    cbind(as.matrix(x$L), dir = x$dir, rhs = x$rhs)
}
#' @export
as.array.lp_constraint <- function(x, ...) {
    as.matrix.lp_constraint(x)
}
#' @export
dimnames.lp_constraint <- function(x) {
    info <- lpsugar_attributes(x)
    list(info$index, NULL)
}
#' @export
head.lp_constraint <- function(x, n = 6L, ...) {
    rlang::check_dots_empty()
    stopifnot(rlang::is_integerish(n, n = 1L, finite = TRUE))
    m <- min(length(x), n)
    x[seq_len(m), ]
}
#' @export
`[.lp_constraint` <- function(x, ..., drop = FALSE) {
    warn_changed_args(drop = FALSE)
    dots <- rlang::dots_list(..., .preserve_empty = TRUE, .ignore_empty = "none")
    
    if (is_empty_constraint(x)) {
        cli_abort(
            "Cannot index an empty constraint.",
            class = "lpsugar_error_index_empty_constraint"
        )
    }
    
    wrong_index <-
        length(dots) == 0L ||
        rlang::is_missing(dots[[1L]]) ||
        length(dots) > 2L ||
        (length(dots) == 2L && !rlang::is_missing(dots[[2L]]))
    
    if (wrong_index) {
        cli_abort(
            "Index constraints with `con[i]` or `con[i, ]`",
            class = "lpsugar_error_bad_constraint_index"
        )
    }
    
    i <- dots[[1L]]
    info <- lpsugar_attributes(x)
    
    if (is.character(i)) {
        undefined <- setdiff(i, info$id)
        
        if (length(undefined) > 0L) {
            cli_abort(
                c("Cannot index constraints that haven't been defined.",
                  "x" = "Undefined constraints: {.str {undefined}}"),
                class = "lpsugar_error_undefined_constraint"
            )
        }
        
        i <- info$id %in% i
    }
    
    x$rhs <- x$rhs[i]
    x$dir <- x$dir[i]
    
    if (inherits(x, "L_constraint")) {
        x$L <- x$L[i, ]
        attr(x, "n_L_constraints") <- length(x$dir)
    }
    else if (inherits(x, "Q_constraint")) {
        x$Q <- x$Q[i]
        x$L <- x$L[i, ]
        attr(x, "n_Q_constraints") <- length(x$dir)
    }
    else if (inherits(x, "F_constraint")) {
        x$F <- x$F[i]
        attr(x, "n_F_constraints") <- length(x$dir)
    }
    else {
        cli_abort(
            "Unsupported constraint class <{roi_constraint_class(x)}>.",
            class = "lpsugar_error_unsupported_constraint_class"
        )
    }
    
    info$id <- info$id[i]
    info$index <- info$index[i]
    info$expr <- info$expr[i]
    
    lpsugar_attributes(x) <- info
    
    if (length(x) == 0L) {
        return(empty_constraint(ncol(x)))
    }
    else {
        return(x)
    }
}

#' @export 
print.empty_lp_constraint <- function(x, ...) {
    cat("No constraints have been defined.")
    invisible(x)
}

#' @export
print.lp_constraint <- function(x, full = TRUE, ...) {
    NextMethod()
    cat("\n")
    
    info <- lpsugar_attributes(x)
    
    conditions_for_full <- all(
        inherits(x, "L_constraint") || inherits(x, "Q_constraint"),
        ncol(x$L) <= 20
    )
    
    if (!conditions_for_full) {
        full <- FALSE
    }
    
    pairs <- data.frame(id = info$id, expr = info$expr) |> 
        unique()
    
    grey_bar <- cli::col_grey("| ")
    
    if (nrow(pairs) > 0L) for (i in 1:nrow(pairs)) {
        id <- pairs$id[i]
        expr <- pairs$expr[i]
        where <- info$id == id & info$expr == expr
        n <- sum(where)
        
        if (id == "") {
            id <- "#unnamed_constraint"
        }
        
        cat(
            id, "\n",
            grey_bar, expr, "\n",
            grey_bar, "Rows = ", n, "\n\n",
            sep = ""
        )
        
        is_q <- is_quadratic(x[where])
        
        if (full && !is_q) {
            mat <- as.matrix.lp_constraint(x[where])
            rownames(mat) <- paste0("  ", rownames(mat))
            print(mat, quote = FALSE)
            cat("\n")
        }
    }
    
    invisible(x)
}
