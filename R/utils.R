
# Messages --------------------------

# Gives a warning if a function argument is not the default
# Useful for arguments that are ignored
warn_changed_args <- function(..., env = parent.frame(), call = env) {
    expected <- rlang::enexprs(..., .named = TRUE, .homonyms = "error")
    
    for (arg in names(expected)) {
        if (!exists(arg, envir = env, inherits = FALSE)) {
            cli_warn("Internal warning: unexistant argument `{arg}`.", call = env)
            next
        }
        
        if (!identical(expected[[arg]], env[[arg]])) {
            cli_warn("Ignoring argument `{arg}`.", call = call)
        }
    }
}

# Safety ------------------------

# Formats an object to a string (length 1)
format1 <- function(x, ...) {
    if (!rlang::is_symbolic(x)) {
        return(rlang::as_label(x))
    }
    
    y <- format(x, ...)
    
    if (length(y) == 1L) {
        y
    } 
    else if (endsWith(y[1], "{")) {
        paste(y[1], "... }")
    } 
    else {
        paste(y[1], "...")
    }
}

dim2 <- function(x) {
    if (is.null(dim(x))) {
        length(x)
    } 
    else {
        dim(x)
    }
}
# Number of dimensions
ndim <- function(x, drop = FALSE) {
    if (drop) {
        # Without max(1), scalars would return 0 dimensions
        sum(dim2(x) > 1L) |> max(1)
    } 
    else {
        length(dim2(x))
    }
}

# Returns FALSE if dimensions are incompatible
# and a condition
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
    
    if (length(x) > 1L && length(y) > 1L && length(x) != length(y)) {
        cnd <- rlang::error_cnd(
            class = "lpsugar_error_non_conformable",
            message = "non-conformable arrays"
        )
        return(structure(FALSE, cnd = cnd))
    }
    
    attempt <- rlang::try_fetch(x + y, warning = identity, error = identity)
    
    if (rlang::is_error(attempt)) {
        attempt$class <- "lpsugar_error_non_conformable"
        return(structure(FALSE, cnd = attempt))
    } 
    else {
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

# Returns a named list with the variable's values
# in their proper dimensions
variables_to_list <- function(x, problem, 
                              binary_as_logical = FALSE, 
                              miss_error = TRUE, 
                              call = environment(), 
                              field = "x", ...) {
    UseMethod("variables_to_list")
}

#' @export
variables_to_list.default <- function(x, problem, 
                                      binary_as_logical = FALSE, 
                                      miss_error = TRUE, 
                                      call = environment(), 
                                      field = "x", ...) {
    if (miss_error && anyNA(x)) {
        cli_abort("`{field}` must not contain NA values.")
    }
    
    purrr::map(problem$variables, function(v) {
        values <- x[v$ind]
        
        if (v$binary && binary_as_logical) {
            values <- values > 0.5
        }
        
        if (v$scalar) {
            unname(values)
        } 
        else {
            array(values, dim = dim(v), dimnames = dimnames(v))
        }
    })
}

#' @export
variables_to_list.list <- function(x, problem, 
                                   binary_as_logical = FALSE, 
                                   miss_error = TRUE, 
                                   call = environment(), 
                                   field = "x", ...) {
    
    purrr::map(problem$variables, function(v) {
        xs <- x[[v$name]]
        
        if (is.null(xs)) {
            if (miss_error) {
                cli_abort(
                    c("`{field}` should include all variables",
                      "x" = "Missing variable: `{v$name}`."),
                    class = "lpsugar_error_missing_variable",
                    call = call
                )
            } 
            else {
                xs <- v$ind
                xs[] <- NA_real_
                return(xs)
            }
        }
        
        if (miss_error && anyNA(xs)) {
            cli_abort(
                "`{field}` must not contain NA values.", 
                class = "lpsugar_error_na_values",
                call = call
            )
        }
        
        return(xs)
    })
}

# Returns a named vector with the values of the variables
# Named with variable.names(problem)
variables_to_vec <- function(x, problem, miss_error = TRUE, 
                             call = environment(), field = "x") {
    UseMethod("variables_to_vec")
}

#' @export
variables_to_vec.default <- function(x, problem, miss_error = TRUE, 
                                     call = environment(), field = "x") {
    x <- as.numeric(x)
    
    if (miss_error && anyNA(x)) {
        cli_abort(
            "`{field}` must not contain NA values.", 
            class = "lpsugar_error_na_values",
            call = call
        )
    }
    
    if (length(x) != ncol(problem)) {
        n <- ncol(problem)
        m <- length(x)
        cli_abort(
            "`problem` has {n} variables but `{field}` is length {m}.",
            class = "lpsugar_error_length_mismatch",
            call = call
        )
    }
    
    names(x) <- attr(problem, "varnames")
    x
}

#' @export
variables_to_vec.list <- function(x, problem, miss_error = TRUE, 
                                  call = environment(), field = "x") {
    solution_vec <- numeric(ncol(problem))
    names(solution_vec) <- attr(problem, "varnames")
    
    for (v in problem$variables) {
        xs <- x[[v$name]]
        
        if (is.null(xs)) {
            if (miss_error) {
                cli_abort(
                    c("`{field}` should include all variables",
                      "x" = "Missing variable: `{v$name}`."),
                    class = "lpsugar_error_missing_variable",
                    call = call
                )
            } 
            else {
                xs <- v$ind
                xs[] <- NA_real_
                solution_vec[v$ind] <- xs
                next
            }
        }
        
        xs <- as.array(xs)
        xs[] <- as.numeric(xs)
        
        if (miss_error && anyNA(xs)) {
            cli_abort(
                "`{field}` must not contain NA values.", 
                class = "lpsugar_error_na_values",
                call = call
            )
        }
        
        dx <- dim(drop(xs))
        dv <- dim(drop(v$ind))
        
        if (any(dx != dv)) {
            dx_str <- paste(dim(xs), collapse = ", ")
            dv_str <- paste(dim(v), collapse = ", ")
            
            cli_abort(
                c("Dimensions of variable `{v$name}` do not match.",
                  "x" = "In `{field}` they are ({dx_str})",
                  "x" = "In `problem` they are ({dv_str})"), 
                class = "lpsugar_error_dimension_mismatch",
                call = call
            )
        }
        else if (length(xs) != length(v)) {
            cli_abort(
                c("Length of variable `{v$name}` does not match.",
                  "x" = "In `{field}` it's length {length(xs)}",
                  "x" = "In `problem` it's length {length(v)}"),
                class = "lpsugar_error_length_mismatch",
                call = call
            )
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
                    class = "lpsugar_error_dimnames_mismatch",
                    call = call
                )
            }
        } 
        
        
        if (v$integer && !rlang::is_integerish(xs[!is.na(xs)])) {
            cli_warn(
                "`{v$name}` should be integer.", 
                class = "lpsugar_warning_non_integer",
                call = call
            )
        }
        
        solution_vec[v$ind] <- xs
    }
    
    solution_vec
}

#' @export
variables_to_vec.lp_solution <- function(x, problem, miss_error = TRUE, 
                                         call = environment(), field = "x") {
    var_vec <- unlist(x$variables)
    true_vec <- x$variables_vec
    
    if (length(var_vec) != length(true_vec) || any(var_vec != true_vec)) {
        cli_abort(
            c("`{field}$variables` and `{field}$variables_vec` do not match.",
              "i" = "You can use either one of them in this function."),
            class = "lpsugar_error_inconsistent_lp_soltution",
            call = call
        )
    }
    
    true_vec
}

# Quadratic ---------------------

# Returns or builds quadratic part of a variable or objective function
get_Q <- function(x) {
    if (is_quadratic(x)) {
        return(x$Q)
    }
    
    qmat <- matrix(
        0,
        nrow = ncol(x$L),
        ncol = ncol(x$L),
        dimnames = list(
            colnames(x$L),
            colnames(x$L)
        )
    )
    
    list(qmat) |> rep(length(x))
}

# Is a variable, constraint, or objective function quadratic?
is_quadratic <- function(x) {
    if (is_lp_variable(x) || is_lp_objective(x)) {
        return(!is.null(x$Q))
    } 
    else if (is_lp_constraint(x)) {
        return(any(lengths(x$Q) > 0L))
    } 
    else {
        return(FALSE)
    }
}

# Turn a variable or objective function into quadratic
as_quadratic <- function(x) {
    if (is_quadratic(x)) {
        return(x)
    }
    if (!is_lp_variable(x) && !is_lp_objective(x)) {
        cli_abort(
            "`x` must be a variable or an objective function.",
            class = "lpsugar_error_internal"
        )
    }
    
    x$Q <- get_Q(x)
    return(x)
}


# Inheritance -------------------

is_problem <- function(x) {
    inherits(x, "lp_problem")
}
is_lp_variable <- function(x) {
    inherits(x, "lp_variable")
}
is_transformed_lp_variable <- function(x) {
    inherits(x, "transformed_lp_variable")
}
is_lp_objective <- function(x) {
    inherits(x, "lp_objective")
}
is_lp_constraint <- function(x, empty_valid = TRUE) {
    is_con <- inherits(x, "lp_constraint")
    if (empty_valid) {
        is_con
    } 
    else {
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
            class = "lpsugar_error_invalid_solution",
            call = parent.frame()
        )
    }
}


# Evaluation --------------------

# Where lp_eval() is evaluated
data_mask <- function(.problem) {
    fun <- custom_fun()
    var <- rlang::new_environment(.problem$variables, parent = fun)
    als <- rlang::new_environment(.problem$aliases, parent = var)
    
    rlang::new_data_mask(bottom = als, top = fun)
}

# Eval an expression inside lp_min(), lp_con(), lp_alias(), ...
lp_eval <- function(.problem, expr, split_for = FALSE) {
    quosure <- rlang::enquo(expr)
    data <- data_mask(.problem)
    
    if (split_for) {
        for_split(quosure, data = data)
    } 
    else {
        rlang::eval_tidy(quosure, data = data)
    }
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
