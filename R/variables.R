
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
#' @param lower Numeric scalar. Lower bound for the variable.
#' @param upper Numeric scalar. Upper bound for the variable.
#'
#' @details
#' Bounds must be numeric scalars. If you want to bind each index of the variable separately,
#' use [lp_constraint()].
#'
#' @returns An [lp_problem()] object with an added variable in `$variables`.
#' @export
#'
#' @examples
lp_variable <- function(.problem, definition,
                        integer = FALSE, binary = FALSE,
                        lower = -Inf, upper = +Inf) {

    check_problem(.problem)

    def <- parse_variable_definition(
        !!rlang::enexpr(definition),
        envir = parent.frame()
    )

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

    # Check if they are numeric scalars, warns if they're `NA` or `NULL`.
    lower <- interpret_bound(lower, default = -Inf)
    upper <- interpret_bound(upper, default = +Inf)

    if (lower > upper) {
        abort("Lower bound ({lower}) is greater than upper bound ({upper}).")
    } else if (lower == +Inf) {
        abort("Lower bound cannot be +Inf.")
    } else if (upper == -Inf) {
        abort("Upper bound cannot be -Inf.")
    }

    if (binary) {
        if (lower != -Inf || upper != +Inf) {
            warn("Ignoring bounds for binary variable `{name}`.")
        }

        integer <- FALSE
        lower <- 0
        upper <- 1
    }

    type <- if (binary)
        "binary"
    else if (integer)
        "integer"
    else
        "real"

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

    if (x$lower != -Inf && x$upper != +Inf) {
        cat("\n")
        cat(x$lower, "<=", x$name, "<=", x$upper)
    } else if (x$lower != -Inf) {
        cat("\n", x$name, " >= ", x$lower, sep = "")
    } else if (x$upper != +Inf) {
        cat("\n", x$name, " <= ", x$upper, sep = "")
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
t.lp_variable <- function(x) {
    # if (ndim(x) > 2L) {
    #     x$ind <- drop(x$ind)
    # }

    if (ndim(x) != 2L) {
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

parse_variable_definition <- function(definition, envir = parent.frame()) {
    def <- rlang::enexpr(definition) |> inside()
    error_msg <- "Failed to parse variable. See ?lp_variable for details on how to define a variable"

    if (rlang::is_symbol(def)) {
        name <- def |> format()
        sets <- list(scalar = "")
        return(list(name = name, sets = sets))

    } else if (def[[1]] == quote(`[`)) {
        name <- def[[2]]

        if (!rlang::is_symbol(name)) {
            abort(error_msg, call = parent.frame())
        }

        name <- format(name)
        sets_exprs <- as.list(def[3:length(def)])

        sets_names <- rlang::names2(sets_exprs)
        unnamed <- sets_names == ""
        sets_names[unnamed] <- sets_exprs[unnamed] |> sapply(format)

        for (s in sets_exprs) if (rlang::is_missing(s)) {
            abort("Sets in `{name}[...]` cannot be missing.", call = parent.frame())
        }

        sets <- sets_exprs |>
            lapply(eval, envir = envir) |>
            rlang::set_names(sets_names)

        return(list(name = name, sets = sets))
    }

    abort(error_msg, call = parent.frame())
}
interpret_bound <- function(bound, default) {
    if (length(bound) > 1L) {
        abort("Lower and upper bounds must be numeric scalars.", call = parent.frame())
    }
    if (length(bound) == 0L || is.na(bound)) {
        warn("Bound is `NA` or `NULL`, setting to {default}.", call = parent.frame())
        return(default)
    }
    if (!is.numeric(bound)) {
        abort("Lower and upper bounds must be numeric scalars.", call = parent.frame())
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

