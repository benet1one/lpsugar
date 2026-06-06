
# Messages --------------------------

warn_changed_args <- function(..., env = parent.frame(), call = env) {
    expected <- rlang::enexprs(..., .named = TRUE, .homonyms = "error")

    for (arg in names(expected)) {
        if (!exists(arg, envir = env, inherits = FALSE)) {
            cli_warn("Internal warning: unexistant argument `{arg}`.", call = env)
            next
        }

        if (expected[[arg]] != env[[arg]]) {
            cli_warn("Ignoring argument `{arg}`.", call = call)
        }
    }
}

# Safety ------------------------

format1 <- function(x, ...) {
    if (!rlang::is_symbolic(x)) {
        return(rlang::as_label(x))
    }

    y <- format(x, ...)

    if (length(y) == 1L) {
        y
    } else if (endsWith(y[1], "{")) {
        paste(y[1], "... }")
    } else {
        paste(y[1], "...")
    }
}
dim2 <- function(x) {
    if (is.null(dim(x))) {
        length(x)
    } else {
        dim(x)
    }
}
ndim <- function(x, drop = FALSE) {
    if (drop) {
        # Without max(1), scalars would return 0 dimensions
        sum(dim2(x) > 1L) |> max(1)
    } else {
        length(dim2(x))
    }
}
non_conformable <- function(x, y) {
    length(x) > 1L && length(y) > 1L && length(x) != length(y)
}
check_conformable <- function(x, y, call = environment()) {
    if (non_conformable(x, y)) {
        cli_abort("Non-conformable arrays", call = call)
    }
}
compatible_dimensions <- function(x, y, drop_dim = TRUE) {
    if (is_lp_variable(x)) {
        x <- x$ind
    }
    if (is_lp_variable(y)) {
        y <- y$ind
    }

    if (drop_dim) {
        x <- drop(x)
        y <- drop(y)
    }

    attempt <- rlang::try_fetch(x + y, warning = identity, error = identity)

    if (rlang::is_error(attempt)) {
        return(structure(FALSE, cnd = attempt))
    } else {
        return(TRUE)
    }
}
dimnames_non_numeric <- function(dimnames) {
    for (i in seq_along(dimnames)) {
        d <- dimnames[[i]]
        if (is.numeric(d) && all(d == seq_along(d))) {
            dimnames[i] <- list(NULL)
        }
    }
    
    return(dimnames)
}

# Transforming variables -------------------------

variables_to_list <- function(x, problem, 
                              binary_as_logical = FALSE, 
                              miss_error = TRUE, 
                              call = environment(), ...) {
    UseMethod("variables_to_list")
}

#' @export
variables_to_list.default <- function(x, problem, 
                                      binary_as_logical = FALSE, 
                                      miss_error = TRUE, 
                                      call = environment(), ...) {
    if (miss_error && anyNA(x)) {
        cli_abort("`x` must not contain NA values.")
    }
    
    purrr::map(problem$variables, function(v) {
        values <- x[v$ind]
        
        if (v$binary && binary_as_logical) {
            values <- values > 0.5
        }
        
        if (v$scalar) {
            unname(values)
        } else {
            array(values, dim = dim(v), dimnames = dimnames(v))
        }
    })
}

#' @export
variables_to_list.list <- function(x, problem, 
                                   binary_as_logical = FALSE, 
                                   miss_error = TRUE, 
                                   call = environment(), ...) {
    
    purrr::map(problem$variables, function(v) {
        xs <- x[[v$name]]
        
        if (is.null(xs)) {
            if (miss_error) {
                cli_abort("Variable `{v$name}` not present in `x`.", call = call)
            } else {
                xs <- v$ind
                xs[] <- NA_real_
                return(xs)
            }
        }
        
        if (miss_error && anyNA(xs)) {
            cli_abort("`x` must not contain NA values.", call = call)
        }
        
        return(xs)
    })
}


variables_to_vec <- function(x, problem, miss_error = TRUE, call = environment()) {
    UseMethod("variables_to_vec")
}

#' @export
variables_to_vec.default <- function(x, problem, miss_error = TRUE, call = environment()) {
    x <- as.numeric(x)
    
    if (miss_error && anyNA(x)) {
        cli_abort("`x` must not contain NA values.", call = call)
    }
    
    if (length(x) != ncol(problem)) {
        n <- ncol(problem)
        m <- length(x)
        cli_abort(
            "`problem` has {n} variables but `x` is length {m}.",
            call = call
        )
    }
    
    names(x) <- attr(problem, "varnames")
    x
}

#' @export
variables_to_vec.list <- function(x, problem, miss_error = TRUE, call = environment()) {
    solution_vec <- numeric(ncol(problem))
    names(solution_vec) <- attr(problem, "varnames")
    
    for (v in problem$variables) {
        xs <- x[[v$name]]
        
        if (is.null(xs)) {
            if (miss_error) {
                cli_abort("Variable `{v$name}` not present in `x`.", call = call)
            } else {
                xs <- v$ind
                xs[] <- NA_real_
                solution_vec[v$ind] <- xs
                next
            }
        }
        
        xs <- as.array(xs)
        xs[] <- as.numeric(xs)
        
        if (miss_error && anyNA(xs)) {
            cli_abort("`x` must not contain NA values.", call = call)
        }

        dx <- dim(drop(xs))
        dv <- dim(drop(v$ind))
        
        if (any(dx != dv)) {
            dx_str <- paste(dim(xs), collapse = ", ")
            dv_str <- paste(dim(v), collapse = ", ")
            cli_abort(
                c("Dimensions of variable `{v$name}` do not match.",
                  "x" = "In `x` they are ({dx_str})",
                  "x" = "In `problem` they are ({dv_str})"), 
                call = call)
        }
        
        dnx <- dimnames(drop(xs))
        dnv <- dimnames(drop(v$ind))
        
        if (!is.null(dnx) && !is.null(dnv)) for (i in seq_along(dnx)) {
            dnxi <- dnx[[i]]
            dnvi <- dnv[[i]]
            
            if (is.null(dnxi) || is.null(dnvi)) {
                next
            }
            
            if (any(dnxi != dnvi)) {
                dn_name <- names(dnv)[i]
                cli_abort(
                    c("Dimension names of variable `{v$name}` do not match.",
                      "!" = "In dimension '{dn_name}'",
                      ">" = "Make sure they are the same names in the same order."),
                    call = call
                )
            }
        }
        
        if (v$integer && !rlang::is_integerish(xs[!is.na(xs)])) {
            cli_warn("`{v$name}` should be integer.", call = call)
        }
        
        solution_vec[v$ind] <- xs
    }
    
    solution_vec
}

#' @export
variables_to_vec.lp_solution <- function(x, problem, miss_error = TRUE, call = environment()) {
    var_vec <- variables_to_vec(x$variables, problem, miss_error = miss_error, call = call)
    true_vec <- x$variables_vec
    
    if (any(var_vec != true_vec)) {
        cli_abort(
            c("`x$variables` and `x$variables_vec` do not match.",
              "i" = "You can use either one of them in this function."),
            call = call
        )
    }
    
    true_vec
}

# Inheritance -------------------

is_problem <- function(x) {
    inherits(x, "lp_problem")
}
is_lp_variable <- function(x) {
    inherits(x, "lp_variable")
}
is_lp_objective <- function(x) {
    inherits(x, "lp_objective")
}
is_lp_constraint <- function(x, empty_valid = TRUE) {
    is_con <- inherits(x, "lp_constraint")
    if (empty_valid) {
        is_con
    } else {
        is_con && !is_empty_lp_constraint(x)
    }
}
is_empty_lp_constraint <- function(x) {
    inherits(x, "empty_lp_constraint")
}
is_lp_solution <- function(x) {
    inherits(x, "lp_solution")
}

check_problem <- function(problem, field_name = ".problem") {
    if (!is_problem(problem)) {
        cli_abort("`{field_name}` must be an `lp_problem`.", call = parent.frame())
    }
}
check_roi_solution <- function(solution, field_name = "solution") {
    expected_fields <- c("solution", "objval", "status", "message")

    if (!is.list(solution) || !all(expected_fields %in% names(solution))) {
        cli_abort(
            "`{field_name}` must be the output of `solve_model()` or `ROI::ROI_solve()`.",
            call = parent.frame()
        )
    }
}


# Evaluation --------------------

data_mask <- function(.problem) {
    fun <- custom_fun()
    var <- rlang::new_environment(.problem$variables, parent = fun)
    als <- rlang::new_environment(.problem$aliases, parent = var)

    rlang::new_data_mask(bottom = als, top = fun)
}
lp_eval <- function(.problem, expr, split_for = FALSE) {
    quosure <- rlang::enquo(expr)
    data <- data_mask(.problem)

    if (split_for) {
        for_split(quosure, data = data)
    } else {
        rlang::eval_tidy(quosure, data = data)
    }
}

inside <- function(expr) {
    if (!rlang::is_symbolic(expr)) {
        return(expr)
    }

    if (rlang::is_quosure(expr)) {
        env <- rlang::quo_get_env(expr)
        expr <- rlang::quo_get_expr(expr)
        expr <- inside(expr)
        return(rlang::new_quosure(expr, env))
    }

    if (rlang::is_symbol(expr)) {
        return(expr)
    }

    if (expr[[1L]] == quote(`(`)) {
        return(expr[[2]])
    }

    if (expr[[1L]] == quote(`{`) && length(expr) == 2L) {
        return(expr[[2]])
    }

    expr
}

# Printing ------------------------

print_field_name <- function(name) {
    cat(
        cli::col_grey(cli::symbol$en_dash),
        cli::style_bold(" $", name),
        "\n",
        sep = ""
    )
}

print_field <- function(x, field, ...) {
    print_field_name(field)
    print(x[[field]], ...)
}
