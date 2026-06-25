
# Sum For ---------------------------------------

#' Index Based Summation
#'
#' Sum over one or more indexing variables.
#'
#' @param ... The first argument(s) must be named: the name represents the indexing variable,
#' and the values represent the sequence over which to sum.
#'
#' The last argument is the expression of the sum.
#'
#' @details
#' The syntax is similar to the one used in math. For instance,
#' \eqn{ \sum_{i=1}^n \sum_{j=1}^m {x_{ij} * c_j} }
#' would be written as
#' `sum_over(i = 1:n, j = 1:n, x[i,j] * c[j])`.
#'
#' @returns The value of the sum.
#' @export
#'
#' @examples
#' cost <- c(5, 2, 7)
#' p <- lp_problem() |>
#'   lp_variable(x[1:2, 1:3]) |>
#'   lp_minimize(sum_over(i = 1:2, j = 1:3, x[i, j] * cost[j]))
#' p$objective
sum_over <- function(...) {
    dots <- rlang::enquos(..., .homonyms = "error")
    nams <- rlang::names2(dots)
    n <- length(dots)
    
    if (nams[n] != "") {
        cli_abort(
            "Last element in `...` should be an unnamed expression to sum.",
            class = "lpsugar_error_last_element_named"
        )
    }
    if (nams[1L] == "") {
        cli_abort(
            "First element in `...` must be a name value pair `<ind> = <set>`",
            class = "lpsugar_error_first_elements_unnamed"
        )
    }
    
    env <- rlang::get_env(dots[[n]])
    expr <- rlang::get_expr(dots[[n]])
    
    listcomp::gen_list(!!expr, !!!dots[-n], .env = env) |>
        purrr::map(sum) |>
        purrr::reduce(`+`, .init = 0)
}


# Methods ---------------------------------------

#' @export
sum.lp_variable <- function(x, ..., na.rm = FALSE) {
    stopifnot(rlang::is_bool(na.rm))
    
    varnames <- colnames(x$L)
    x$ind <- x$ind[1]
    
    if (is_quadratic(x)) {
        x$Q <- list(purrr::reduce(x$Q, `+`))
    }
    
    x$L <- colSums(x$L) |>
        matrix(nrow = 1L) |>
        robust_index()
    x$A <- sum(x$A) |>
        matrix(nrow = 1L, ncol = 1L) |>
        robust_index()
    
    colnames(x$L) <- varnames
    
    if (...length() > 0L) {
        x <- x + sum(..., na.rm = na.rm)
    }
    
    transformed_variable(x)
}
#' @export
mean.lp_variable <- function(x, ...) {
    rlang::check_dots_empty(...)
    sum(x) / length(x)
}

#' @export
Math.lp_variable <- function(x, ...) {
    fun <- .Generic
    call <- paste0(fun, "(", format(substitute(x)), ")") |>
        str2lang()
    
    if (fun == "cumsum") {
        return(cumsum_v(x, call))
    }
    
    if (fun == "abs") {
        abs_url <- "https://optimization.cbe.cornell.edu/index.php?title=Optimization_with_absolute_values"
        message <- paste(
            "Function `abs()` is not linear.",
            "See how to implement absolute values in linear programming here:",
            "{.url {abs_url}}"
        )
        cli_abort(message, call = call, class = "lpsugar_error_abs")
    }
    
    cli_abort(
        "Function `{fun}()` is not supported in a linear problem.", 
        class = "lpsugar_error_unsupported_math",
        call = call
    )
}

cumsum_v <- function(x, call) {
    if (length(x) >= 2L) for (i in 2:length(x)) {
        if (is_quadratic(x)) {
            x$Q[[i]] <- x$Q[[i]] + x$Q[[i-1L]]
        }
        x$L[i, ] <- x$L[i, ] + x$L[i-1L, ]
    }
    
    x$A[] <- cumsum(x$A)
    transformed_variable(x)
}


# Custom Function Data Mask ---------------------

# Overwrites base functions to make the applicable
# to lp_variables
custom_fun <- function() {
    e <- rlang::env()
    
    e$diag <- function(x = 1, nrow, ncol, names = TRUE) {
        if (!is_lp_variable(x)) {
            return(base::diag(x, nrow, ncol, names))
        }
        
        warn_changed_args(nrow = , ncol = , names = TRUE)
        diag_v(x)
    }
    
    # sum.lp_variable() only works if first element in `...` is an lp_variable
    # this allows the first element to be anything
    e$sum <- function(..., na.rm = FALSE) {
        rlang::dots_list(...) |>
            purrr::map(base::sum, na.rm = na.rm) |>
            purrr::reduce(`+`)
    }
    
    # If weighted.mean is a method, it only covers the case where x is a variable
    # and not when w is a variable (which should throw an error because it's not linear)
    e$weighted.mean <- function(x, w, ..., na.rm = FALSE) {
        if (!missing(w) && is_lp_variable(w)) {
            cli_abort(
                "Weights `w` cannot be a variable", 
                class = "lpsugar_error_weigth_is_variable"
            )
        }
        if (is_lp_variable(x)) {
            rlang::check_dots_empty()
            warn_changed_args(na.rm = FALSE)
            na.rm <- FALSE
        }
        
        return(stats::weighted.mean(x, w, ..., na.rm))
    }
    
    e$ifelse <- function(test, yes, no) {
        if (is_lp_constraint(test)) {
            test_expr <- substitute(test) |> rlang::as_label()
            cli_abort(
                c("The `test` condition must be a binary variable, not an equality or inequality.",
                  "x" = "Problematic argument: {test_expr}"),
                class = "lpsugar_error_ifelse_with_constraints"
            )
        }
        
        if (is_lp_constraint(yes) || is_lp_constraint(no)) {
            cli_abort(
                "`yes` and `no` cannot be constraints.",
                class = "lpsugar_error_ifelse_with_constraints"
            )
        }
        
        if (is_lp_variable(test)) {
            ifelse_v(test, yes, no)
        } 
        else if (is_lp_variable(yes) || is_lp_variable(no)) {
            ifelse_l(test, yes, no)
        } 
        else {
            base::ifelse(test, yes, no)
        }
    }
    
    e$apply <- function(X, MARGIN, FUN, ..., simplify = TRUE) {
        if (!is_lp_variable(X)) {
            return(base::apply(X, MARGIN, FUN, ..., simplify = simplify))
        }
        
        warn_changed_args(simplify = TRUE)
        apply_v(X, MARGIN, FUN, ...)
    }
    
    e$rowSums <- function(x, na.rm = FALSE, dims = 1L) {
        if (!is_lp_variable(x)) {
            return(base::rowSums(x, na.rm, dims = dims))
        }
        
        warn_changed_args(na.rm = FALSE, dims = 1L)
        apply_v(x, 1L, sum)
    }
    e$colSums <- function(x, na.rm = FALSE, dims = 1L) {
        if (!is_lp_variable(x)) {
            return(base::colSums(x, na.rm, dims = dims))
        }
        
        warn_changed_args(na.rm = FALSE, dims = 1L)
        apply_v(x, 2L, sum)
    }
    e$rowMeans <- function(x, na.rm = FALSE, dims = 1L) {
        if (!is_lp_variable(x)) {
            return(base::rowMeans(x, na.rm, dims = dims))
        }
        
        warn_changed_args(na.rm = FALSE, dims = 1L)
        apply_v(x, 1L, mean)
    }
    e$colMeans <- function(x, na.rm = FALSE, dims = 1L) {
        if (!is_lp_variable(x)) {
            return(base::colMeans(x, na.rm, dims = dims))
        }
        
        warn_changed_args(na.rm = FALSE, dims = 1L)
        apply_v(x, 2L, mean)
    }
    e$marginSums <- function(x, margin = NULL) {
        if (!is_lp_variable(x)) {
            return(base::marginSums(x, margin = margin))
        }
        if (length(margin) == 0) {
            return(sum.lp_variable(x))
        }
        
        apply_v(x, margin, sum)
    }
    
    return(e)
}

# diag() of an lp_variable
diag_v <- function(x) {
    if (ndim(x) != 2L) {
        cli_abort(
            "Variable is not two-dimensional.", 
            class = "lpsugar_error_not_2d",
            call = parent.frame()
        )
    }
    
    present_ind <- x$ind
    present_ind[] <- seq_along(present_ind)
    
    if (!is.matrix(present_ind)) {
        cli_abort(
            "Internal error in `diag_v()`. Sorry!",
            class = "lpsugar_error_internal"
        )
    }
    
    present_ind <- base::diag(present_ind)
    x[present_ind]
}

# apply() on an lp_variable
apply_v <- function(x, margin, fun, ..., simplify = TRUE) {
    margin <- parse_margin(margin, variable = x)
    fun <- match.fun(fun)
    simplify <- isTRUE(simplify)
    
    ind <- x$ind
    ind[] <- seq_along(ind)
    ind_list <- apply(ind, margin, identity, simplify = FALSE)
    
    out_list <- purrr::map(ind_list, \(i) fun(x[i], ...))
    
    if (!simplify) {
        return(out_list)
    }
    
    out_list <- unname(out_list)
    out <- bind_vars(!!!out_list)
    
    if (!is_lp_variable(out)) {
        return(simplify2array(out))
    }
    
    out_ind <- apply(ind, margin, fun, ..., simplify = FALSE) |>
        simplify2array(higher = TRUE, except = 0)
    
    if (is.array(out_ind)) {
        out_ind[] <- seq_along(out_ind)
        out$ind <- out_ind |> robust_index()
    } 
    else {
        out$ind <- 1:nrow(out$L)
    }
    
    return(out)
}
parse_margin <- function(margin, variable) {
    if (rlang::is_integerish(margin, finite = TRUE)) {
        return(margin)
    }
    if (rlang::is_character(margin)) {
        if (is_transformed_lp_variable(variable)) {
            cli_abort("Character `MARGIN` is only supported for non-transformed variables.")
        }
        
        dnn <- names(dimnames(variable))
        margin_num <- match(margin, dnn)
        
        if (anyNA(margin_num)) {
            where_na <- margin[is.na(margin_num)][1]
            cli_abort(
                "Margin '{where_na}' does not match any dimension in `{variable$name}`.",
                class = "lpsugar_error_bad_margin"
            )
        }
        
        return(margin_num)
    }
    
    cli_abort("Invalid `MARGIN`.", class = "lpsugar_error_bad_margin")
}

# `test` is lp_variable
ifelse_v <- function(test, yes, no) {
    if (!test$binary) {
        cli_abort(
            "`test` must be a binary variable.",
            class = "lpsugar_error_test_non_binary"
        )
    }
    if (is_lp_variable(yes) || is_lp_variable(no)) {
        cli_abort(
            "If `test` is a variable, `yes` and `no` must be numbers, not variables.",
            class = "lpsugar_error_bad_ifelse"
        )
    }
    
    no + test * (yes - no)
}

# `test` is logical vector, `yes` or `no` are lp_variables
ifelse_l <- function(test, yes, no) {
    # Default R code from `ifelse`
    if (is.atomic(test)) {
        if (typeof(test) != "logical")
            storage.mode(test) <- "logical"
        if (length(test) == 1 && is.null(attributes(test))) {
            if (is.na(test))
                return(NA)
            else if (test) {
                if (length(yes) == 1) {
                    yat <- attributes(yes)
                    if (is.null(yat) || (is.function(yes) && identical(names(yat),
                                                                       "srcref")))
                        return(yes)
                }
            }
            else if (length(no) == 1) {
                nat <- attributes(no)
                if (is.null(nat) || (is.function(no) && identical(names(nat),
                                                                  "srcref")))
                    return(no)
            }
        }
    }
    else test <- if (isS4(test))
        methods::as(test, "logical")
    else as.logical(test)
    
    # lpsugar code
    if (anyNA(test)) {
        cli_abort(
            "`test` cannot contain NA values.",
            class = "lpsugar_error_ifelse_test_na"
        )
    }
    
    len <- length(test)
    
    if (is_lp_variable(yes)) {
        yes <- recycle_var(yes, len)
    } 
    else if (is.numeric(yes) || is.logical(yes)) {
        yes <- recycle_const(yes, len)
    } 
    else {
        cli_abort(
            "`yes` must either be a variable or a numeric vector.",
            class = "lpsugar_error_ifelse_bad_yes"
        )
    }
    
    if (is_lp_variable(no)) {
        no <- recycle_var(no, len)
    } 
    else if (is.numeric(no) || is.logical(no)) {
        no <- recycle_const(no, len)
    } 
    else {
        cli_abort(
            "`no` must either be a variable or a numeric vector.",
            class = "lpsugar_error_ifelse_bad_no"
        )
    }
    
    test <- as.numeric(test)
    
    yes <- yes * test
    no <- no * (1 - test)
    
    yes + no
}
