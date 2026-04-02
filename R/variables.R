
#' Define a variable for an [lp_problem()]
#'
#' Add a variable to a problem by defining it's name, dimensions, type, and bounds.
#'
#' @param .problem An [lp_problem()] object.
#' @param definition Expression.
#' - If the variable is a scalar, simply type it's name.
#'   - `lp_variable(x)`
#'
#' - If the variable is a vector, type it's name and indices. Indices can also be named.
#'   - `lp_variable( v[1:5] )`
#'   - `lp_variable( v[letters[1:5]] )`
#'   - `lp_variable( v[ind = letters[1:5]] )`
#'   - `ind <- letters[1:5]; lp_variable( v[ind] )`
#'
#'   The last two have the same result.
#'
#' - If the variable is a matrix or n-dimensional array, type it's name and the indices
#' for every dimension ([base::dimnames()]), comma-separated.
#'   - `lp_variable( mat[1:2, 1:3] )`
#'   - `lp_variable( arr[1:2, 1:3, 1:2] )`
#'
#' @param integer Boolean, whether to treat variable as integer.
#' @param binary Boolean, whether to treat variable as binary, \{0, 1\}.
#' @param lower Numeric scalar or array. Lower bound for the variable.
#' @param upper Numeric scalar or array. Upper bound for the variable.
#'
#' @returns The `.problem` with an added variable in `$variables`.
#' The fields of `lp_variable` objects are intended for internal use, modifying them is
#' highly discouraged.
#' - `$name` : String, name of the variable.
#' - `$lower` and `$upper` : Bounds.
#' - `$type` : String, one of `"real"`, `"integer"` or `"binary"`.
#' - `$integer` and `$binary` : Booleans. If `$binary` is true, then `$integer` is also true.
#'
#' The following fields are meant for internal use only.
#'
#' - `$ind` : Integer array. Indicates which indices correspond to this variable.
#' Meant for internal use only.
#'
#' - `$coef` : Numeric matrix. The number of rows is the length of the variable, the number
#' of columns is the total amount of variables in the problem.
#' The values represent coefficients that are modified when adding variables or multiplying
#' by a constant. Used for objective, constraints and aliases.
#'
#' - `$add` : Numeric column vector. The number of rows is the length of the variable.
#' The values represent addends, modified when adding the variable and a constant.
#' Used for objective, constraints and aliases.
#'
#' - `$raw` : Boolean indicating if the variable has been modified in any way
#' (indexed, multiplied, summed, ...) or remains as defined.
#'
#' @export
#'
#' @example inst/examples/example_variable.R
lp_variable <- function(.problem, definition,
                        integer = FALSE, binary = FALSE,
                        lower = -Inf, upper = +Inf) {

    check_problem(.problem)

    def <- parse_variable_definition({{ definition }})

    name <- def$name
    sets <- def$sets

    if (name %in% names(.problem$variables)) {
        abort("Variable `{name}` already exists in this problem.")
    } else if (name %in% names(.problem$aliases)) {
        abort("Cannot override alias `{name}`.")
    }

    stopifnot(
        rlang::is_bool(integer),
        rlang::is_bool(binary)
    )

    lower <- interpret_bound(lower, "lower", default = -Inf, dim = lengths(sets))
    upper <- interpret_bound(upper, "upper", default = +Inf, dim = lengths(sets))

    if (any(lower > upper)) {
        abort("`lower` bound ({lower}) cannot be greater than `upper` bound ({upper}).")
    } else if (any(lower == +Inf)) {
        abort("`lower` bound cannot be +Inf.")
    } else if (any(upper == -Inf)) {
        abort("`upper` bound cannot be -Inf.")
    }

    if (binary) {
        if (any(lower > 1)) {
            abort("`lower` bound cannot be greater than 1 for a binary variable.")
        }
        if (any(upper < 0)) {
            abort("`upper` bound cannot be less than 0 for a binary variable.")
        }

        integer <- TRUE
        lower <- pmax(lower, 0)
        upper <- pmin(upper, 1)
    }

    type <- if (binary && all(lower == 0) && all(upper == 1)) {
        "B"
    } else if (binary || integer) {
        "I"
    } else {
        "C"
    }

    # Index array of variable.
    # Indicates which objective coefficients correspond to this variable.
    ind <- array(dim = lengths(sets), dimnames = sets) |> robust_index()
    ind[] <- 1:length(ind) + .problem$.nvar
    .problem$.nvar <- max(ind)

    .problem$.varnames <- c(
        .problem$.varnames,
        name_variable(name, sets)
    )

    add <- matrix(0, nrow = length(ind), ncol = 1L) |> robust_index()
    coef <- matrix(0, nrow = length(ind), ncol = .problem$.nvar) |> robust_index()
    coef[, ind] <- diag(length(ind))

    new_variable <- list(
        name = name,
        lower = lower,
        upper = upper,

        type = type,
        integer = integer,
        binary = binary,

        ind = ind,
        coef = coef,
        add = add,
        raw = TRUE

    ) |> structure(class = "lp_variable")

    .problem$variables <- append(
        .problem$variables,
        list(new_variable) |> rlang::set_names(name)
    )

    .problem <- update_variables(.problem, field = "variables")
    .problem <- update_variables(.problem, field = "aliases")
    .problem <- update_objective(.problem)
    .problem <- update_constraints(.problem)
    .problem
}


update_variables <- function(.problem, field = "variables") {
    total_vars <- .problem$.nvar
    varnames <- .problem$.varnames
    vars <- .problem[[field]]

    for (i in names(vars)) {
        x <- vars[[i]]

        x$coef <- cbind(
            x$coef,
            matrix(0, nrow = nrow(x$coef), ncol = total_vars - ncol(x$coef))
        )

        x$coef <- robust_index(x$coef)
        colnames(x$coef) <- varnames

        vars[[i]] <- x
    }

    .problem[[field]] <- vars
    .problem
}

# Alias ----------------------

#' @rdname lp_variable
#' @export
lp_var <- lp_variable

# Methods --------------------

#' @export
print.lp_variable <- function(x, ...) {
    if (!x$raw) {
        unclass(x)[c("coef", "add")] |> print()
        return(invisible(x))
    }

    if (x$binary) {
        cat("Binary ")
    } else if (x$integer) {
        cat("Integer ")
    } else {
        cat("Real ")
    }

    if (length(x) == 1L) {
        cat("scalar ")
    } else {
        cat("variable ")
    }

    cat("'")
    cat(x$name)

    if (length(x) > 1L) {
        cat("[", paste(names(dimnames(x)), collapse = ", "), "]", sep = "")
    }

    cat("'")

    if (length(x$lower) == 1L && length(x$upper) == 1L) {
        if (x$lower != -Inf && x$upper != +Inf) {
            cat("\n")
            cat(x$lower, "<=", x$name, "<=", x$upper)
        } else if (x$lower != -Inf) {
            cat("\n", x$name, " >= ", x$lower, sep = "")
        } else if (x$upper != +Inf) {
            cat("\n", x$name, " <= ", x$upper, sep = "")
        }
    }

    cat("\n")
    invisible(x)
}
#' @export
dim.lp_variable <- function(x) {
    dim(x$ind)
}
#' @export
length.lp_variable <- function(x) {
    length(x$ind)
}
#' @export
dimnames.lp_variable <- function(x) {
    dimnames(x$ind)
}
#' @export
names.lp_variable <- function(x) {
    names(x$ind)
}
#' @export
c.lp_variable <- function(x, ...) {
    abort("concatenating `lp_variable`s is not supported.")
}
#' @export
as.logical.lp_variable <- function(x, ...) {
    abort("Variables cannot be coerced to type 'logical'.")
}

# Indexing ----------------------

#' @export
`[.lp_variable` <- function(x, ..., drop = FALSE) {
    old_ind <- x$ind
    old_ind[] <- 1:length(old_ind)
    old_ind <- old_ind[..., drop = drop]
    old_ind <- c(old_ind)

    x$ind <- x$ind[..., drop = drop]
    x$raw <- FALSE

    x$coef <- x$coef[old_ind, ]
    x$add <- x$add[old_ind, ]

    x
}
#' @export
`[[.lp_variable` <- function(x, ...) {
    abort("`lp_variable`s don't support double indexing `{x$name}[[i]]`. Use `{x$name}[i]` instead")
}
#' @export
rep.lp_variable <- function(x, ...) {
    ind <- rep(1:length(x), ...)
    x[ind]
}
#' @export
t.lp_variable <- function(x) {
    if (ndim(x) > 2L) {
        msg <- glue::glue(
            "Variable must be two-dimensional. ",
            "Index it with `{x$name}[..., drop = TRUE]` to drop unnecessary dimensions."
        )
        abort(msg)

    } else if (ndim(x) < 2L) {
        abort("Variable must be two-dimensional.")
    }

    present_ind <- x$ind
    present_ind[] <- 1:length(present_ind)
    present_ind <- t(present_ind)
    present_ind <- c(present_ind)

    y <- x[present_ind]
    y$ind <- t(x$ind)

    return(y)
}

# Utils ----------------------

parse_variable_definition <- function(definition) {
    def <- rlang::enquo(definition) |> inside()
    expr <- rlang::get_expr(def)
    env  <- rlang::get_env(def)

    error_msg <- "Failed to parse variable. See ?lp_variable for details on how to define a variable"


    if (rlang::is_string(expr)) {
        expr <- rlang::sym(expr)
        def <- rlang::as_quosure(expr, env = env)
    }

    if (rlang::is_symbol(expr)) {
        name <- expr |> format()
        sets <- list(scalar = "")
        return(list(name = name, sets = sets))

    } else if (expr[[1]] == quote(`[`)) {
        name <- expr[[2]]

        if (!rlang::is_symbol(name) && !rlang::is_string(name)) {
            abort(error_msg, call = parent.frame())
        }

        name <- format(name)
        sets_exprs <- as.list(expr[3:length(expr)])

        sets_names <- rlang::names2(sets_exprs)
        unnamed <- sets_names == ""
        sets_names[unnamed] <- sets_exprs[unnamed] |> sapply(format1)

        for (s in sets_exprs) if (rlang::is_missing(s)) {
            abort("Sets in `{name}[...]` cannot be missing.", call = parent.frame())
        }

        sets <- sets_exprs |>
            lapply(eval, envir = env) |>
            rlang::set_names(sets_names)

        for (s in seq_along(sets)) {
            st <- sets[[s]]
            nd <- ndim(st, drop = TRUE)

            if (nd != 1L) {
                nm <- names(sets)[s]
                warn("set `{nm}` is {nd}-dimensional, results may be unexpected.")
            }
        }

        return(list(name = name, sets = sets))
    }

    abort(error_msg, call = parent.frame())
}
interpret_bound <- function(bound, bound_name, default, dim) {
    if (length(bound) == 0L) {
        warn("`{bound_name}` bound is `NULL` or zero-length, setting to {default}.",
             call = parent.frame())
        return(default)
    }

    if (length(bound) > 1L && !all(dim2(bound) == dim)) {
        abort("`dim({bound_name})` different from `dim(variable)`.",
              call = parent.frame())
    }

    if (anyNA(bound)) {
        warn("`{bound_name}` bound containts NA values, setting to {default}.", call = parent.frame())
        bound[is.na(bound)] <- default
    }

    if (!is.numeric(bound)) {
        abort("`{bound_name}` bound is not numeric.", call = parent.frame())
    }

    bound
}
name_variable <- function(name, sets) {
    if (length(sets) == 1L && lengths(sets) == 1L)
        return(name)
    grid <- do.call(expand.grid, sets)
    index <- .mapply(dots = grid, FUN = paste, MoreArgs = list(sep = ","))
    paste0(name, "[", index, "]")
}

recycle_var <- function(x, n) {
    if (length(x) == n) {
        return(x)
    } else if (length(x) == 1L) {
        i <- rep(1L, n)
        x$ind <- x$ind[i]
        x$coef <- x$coef[i, ]
        x$add <- x$add[i, ]
        x$raw <- FALSE
        return(x)
    }

    abort("Attempt to recycle variable of length ({length(x)}) to length ({n}).")
}
recycle_const <- function(x, n) {
    if (length(x) == n) {
        return(c(x))
    } else if (length(x) == 1L) {
        return(rep(x, n))
    }

    abort("Attempt to recycle array of length ({length(x)}) to length ({n}).")
}

