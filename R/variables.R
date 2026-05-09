
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
    ind[] <- 1:length(ind) + ncol(.problem)
    attr(.problem, "n_variables") <- max(ind)

    attr(.problem, "varnames") <- c(
        attr(.problem, "varnames"),
        name_variable(name, sets)
    )

    add <- matrix(0, nrow = length(ind), ncol = 1L) |> robust_index()
    coef <- matrix(0, nrow = length(ind), ncol = ncol(.problem)) |> robust_index()
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
    total_vars <- ncol(.problem)
    varnames <- attr(.problem, "varnames")
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
        fields <- if (is_quadratic(x)) {
            c(c("q_coef", "coef", "add"))
        } else {
            c("coef", "add")
        }

        unclass(x)[fields] |> print()
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
as.logical.lp_variable <- function(x, ...) {
    abort("Variables cannot be coerced to type 'logical'.")
}

# Concatenation --------------------

#' @export
c.lp_variable <- function(..., recursive = TRUE) {
    rlang::abort(c(
        "Cannot use `c()` to concatenate `lp_variable`s.",
        ">" = "Use `bind_vars()` instead."
    ))
}

#' Concatenate variables and numbers.
#'
#' Bind multiple variables and/or constants together.
#'
#' @param ... Problem variables and/or numeric constants, vectors or arrays.
#' @returns An `lp_variable`.
#' @export
#' @examples
bind_vars <- function(...) {
    dots <- rlang::dots_list(..., .ignore_empty = "all")
    dots_expr <- rlang::enexprs(..., .ignore_empty = "all")

    nams <- rlang::names2(dots)
    if (any(nams != "")) {
        warn("ignoring named arguments in `bind_vars()`")
    }

    len0 <- lengths(dots) == 0
    dots <- dots[!len0]
    dots_expr <- dots_expr[!len0]

    if (length(dots) == 0) {
        return(numeric())
    }

    for (i in seq_along(dots)) {
        x <- dots[[i]]

        if (!is_lp_variable(x) && !is.numeric(x)) {
            e <- dots_expr[[i]] |> format1()
            abort("`{e}` is not numeric or an `lp_variable`.")
        }
        if (is_lp_variable(x) && is_quadratic(x)) {
            e <- dots_expr[[i]] |> format1()
            abort("Quadratic variables such as `{e}` are not currently supported in `bind_vars()`")
        }
    }

    purrr::reduce(dots, function(x, y) {
        xv <- is_lp_variable(x)
        yv <- is_lp_variable(y)

        if (xv && yv) {
            bind_vv(x, y)
        } else if (xv && !yv) {
            bind_vc(x, y)
        } else if (!xv && yv) {
            bind_cv(x, y)
        } else {
            c(x, y)
        }
    })
}
bind_vv <- function(x, y) {
    z <- x
    z$ind  <- seq_len(length(x) + length(y)) |> unname() |> robust_index()
    z$coef <- rbind(x$coef, y$coef) |> robust_index()
    z$add  <- rbind(x$add,  y$add)  |> robust_index()
    z$raw  <- FALSE
    return(z)
}
bind_vc <- function(x, y) {
    x$ind <- c(x$ind, numeric(length(y))) |> unname() |> robust_index()
    x$ind[] <- 1:length(x$ind)

    x$coef <- rbind(
        x$coef,
        matrix(0, nrow = length(y), ncol = ncol(x$coef))
    ) |> robust_index()
    x$add <- rbind(
        x$add,
        matrix(y, ncol = 1L)
    ) |> robust_index()

    x$raw <- FALSE
    return(x)
}
bind_cv <- function(x, y) {
    i <- c(seq_along(x) + length(y), seq_along(y))
    z <- bind_vc(y, x)[i]
    z$ind <- seq_along(i) |> robust_index()
    return(z)
}


# Indexing ----------------------

#' @export
`[.lp_variable` <- function(x, ..., drop = FALSE) {
    old_ind <- x$ind
    old_ind[] <- 1:length(old_ind)

    old_ind <- rlang::try_fetch(old_ind[..., drop = drop], error = identity)

    if (rlang::is_condition(old_ind)) {
        dots <- rlang::enexprs(..., .ignore_empty = "none")
        call <- rlang::expr((!!substitute(x))[!!!dots])
        abort(old_ind$message, call = call)
    }

    old_ind <- c(old_ind)
    x$ind <- x$ind[..., drop = drop]
    x$raw <- FALSE

    x$coef <- x$coef[old_ind, ]
    x$add <- x$add[old_ind, ]

    x
}
#' @export
`[<-.lp_variable` <- function(x, i, j, ..., value) {
    `(<-` <- `!<-` <- NULL
    xname <- rlang::sym(x$name)
    call <- rlang::expr((!!xname)[...] <- {{value}})
    abort("Cannot assign an element of an `lp_variable`.", call = call)
}
#' @export
`[[.lp_variable` <- function(x, ...) {
    rlang::abort(c(
        glue::glue("Double indexing `{x$name}[[i]]` not supported for `lp_variable`."),
        ">" = glue::glue("Use `{x$name}[i]` instead.")
    ))
}
#' @export
`[[<-.lp_variable` <- function(x, i, value) {
    abort("Double indexing `{x$name}[[i]]` not supported for `lp_variable`.")
}

#' @export
rep.lp_variable <- function(x, ...) {
    ind <- rep(1:length(x), ...)
    x[ind]
}
#' @export
t.lp_variable <- function(x) {
    if (ndim(x) > 2L) {
        abort(glue::glue(
            "Variable must be two-dimensional. ",
            "Index it with `{x$name}[..., drop = TRUE]` to drop unnecessary dimensions."
        ))

    } else if (ndim(x) == 1L) {
        x$ind <- matrix(x$ind, ncol = 1L)
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
        sets <- list(scalar = "")
        return(list(name = expr, sets = sets))

    } else if (rlang::is_symbol(expr)) {
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
            check_variable_set(
                set = sets[[s]],
                name = names(sets)[s],
                call = parent.frame()
            )
        }

        return(list(name = name, sets = sets))
    }

    abort(error_msg, call = parent.frame())
}
check_variable_set <- function(set, name, call = environment()) {
    if (!rlang::is_atomic(set)) {
        abort("Set `{name}` is not atomic.", call = call)
    }

    nd <- ndim(set, drop = TRUE)

    if (nd != 1L) {
        warn("Set `{name}` is {nd}-dimensional, results may be unexpected.", call = call)
    }

    if (is.numeric(set) && any(set != seq_along(set))) {
        rlang::abort(c(
            glue::glue("Numeric sets like `{name}` must go from 1 to n."),
            ">" = "Use character sets for more flexibility."
        ))
    }
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

        if (is_quadratic(x)) {
            x$q_coef <- x$q_coef[i]
        }

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

