
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
    inds <- rlang::names2(cons)
    
    indices <- ifelse(
        inds != "", 
        paste0(id, "[", inds, "]"),
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
                "x" = "Problematic constraint: '{name_ind[i]}'.",
                ">" = "Did you forget the comparison operator? `<=/==/>=`"
            )
            
            cli_abort(msg, call = quosure, class = "lpsugar_error_no_constraint")
        }
        
        cons[[i]]$index[] <- indices[i]
        
        if (!is.null(cons[[i]]$L)) {
            rownames(cons[[i]]$L) <- cons[[i]]$index
        }
    }
    
    cons <- bind_cons(!!!cons)
    cons$id[] <- id
    
    return(cons)
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
lp_delete_constraint <- function(.problem, names) {
    cli_abort("TODO")
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

empty_constraint <- function() {
    structure(
        list(),
        class = c("lp_empty_constraint", "lp_constraint")
    )
}

update_constraints <- function(.problem) {
    if (length(.problem$constraints) == 0L) {
        return(.problem)
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
        return(empty_constraint())
    }
    
    roi_binder <- get("rbind.constraint", pos = getNamespace("ROI"))
    out <- rlang::exec(roi_binder, !!!dots)
    class(out) <- c("lp_constraint", class(out))
    
    lpsugar_attributes(out) <- purrr::map(dots, lpsugar_attributes) |> 
        purrr::list_transpose(simplify = FALSE) |> 
        purrr::map(\(x) unlist(x, use.names = FALSE))

    return(out)
}

# Methods ----------------------

#' @export
rbind.lp_constraint <- function(..., deparse.level = 1) {
    warn_changed_args(deparse.level = 1)
    bind_cons(...)
}

#' @export
as.matrix.lp_constraint <- function(x, ...) {
    if (!inherits(x, "L_constraint")) {
        cli_abort(
            c("Can only convert linear constraints into matrices.",
              "x" = "`x` is <{class(x)[2]}>"),
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
length.lp_empty_constraint <- function(x) {
    0
}
#' @export
dim.lp_constraint <- function(x) {
    c(NextMethod(), NA)
}
#' @export
dimnames.lp_constraint <- function(x) {
    list(lpsugar_attributes(x) $ index, NULL)
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
    
    if (is.character(i)) {
        i <- x$name %in% i
    }
    
    x$Q <- x$Q[i]
    x$L <- x$L[i, ]
    x$rhs <- x$rhs[i, ]
    x$dir <- x$dir[i]
    x$call <- x$call[i]
    x$name <- x$name[i]
    
    return(x)
}

#' @export
print.lp_constraint <- function(x, compact = FALSE, ...) {
    NextMethod()
}
