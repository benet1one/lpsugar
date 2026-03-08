
#' Define a variable for a Linear Problem
#'
#' Add a variable to a problem by defining it's name, dimensions, type, and bounds.
#'
#' @param .problem An [lp_problem()] object.
#' @param definition Expression.
#' - If the variable is a scalar, simply type it's name.
#'   - `lp_variable(x)`
#'
#' - If the variable is a vector, type it's name and indices. Indices can also be named. e.g.
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
#' @returns An [lp_problem()] object.
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
    }

    stopifnot(
        rlang::is_bool(integer),
        rlang::is_bool(binary)
    )

    # Corrects NULL and NA bounds as well as checking they are numeric scalars
    lower <- interpret_bound(lower, default = -Inf)
    upper <- interpret_bound(upper, default = +Inf)

    if (lower > upper) {
        abort("Lower bound {lower} is greater than upper bound {upper}.")
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

    add <- matrix(0, ncol = 1L, nrow = length(ind)) |> robust_index()

    new_variable <- list(
        name = name,
        sets = sets,
        bound = c(Lower = lower, Upper = upper),

        type = type,
        integer = integer,
        binary = binary,

        ind = ind,
        coef = NULL, # Created and updated later
        add = add,
        raw = TRUE

    ) |> structure(class = "lp_variable")

    .problem$variables <- append(
        .problem$variables,
        list(new_variable) |> rlang::set_names(name)
    )

    .problem <- update_variables(.problem)
    .problem <- update_objective(.problem)
    .problem <- update_constraints(.problem)
    .problem
}


update_variables <- function(.problem) {
    total_vars <- .problem$.nvar
    varnames <- .problem$.varnames

    for (i in names(.problem$variables)) {
        x <- .problem$variables[[i]]

        x$coef <- matrix(0, nrow = length(x), ncol = total_vars) |> robust_index()
        x$coef[, x$ind] <- diag(length(x))
        colnames(x$coef) <- varnames

        # v$selected <- numeric(total_vars)
        # v$selected[v$ind] <- TRUE

        .problem$variables[[i]] <- x
    }

    .problem
}

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
        cat("[", paste(names(x$sets), collapse = ", "), "]", sep = "")
    }

    cat("'")

    if (all(x$bound != c(-Inf, +Inf))) {
        cat("\n")
        cat(x$bound[1L], "<=", x$name, "<=", x$bound[2L])
    } else if (x$bound[1L] != -Inf) {
        cat("\n", x$name, " >= ", x$bound[1L], sep = "")
    } else if (x$bound[2L] != +Inf) {
        cat("\n", x$name, " <= ", x$bound[2L], sep = "")
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
        abort("variable must be two-dimensional.")
    }

    present_ind <- x$ind
    present_ind[] <- 1:length(present_ind)
    present_ind <- t(present_ind)
    present_ind <- c(present_ind)

    x$ind <- t(x$ind)
    x$raw <- FALSE

    x[present_ind]
}

# Utils ----------------------

parse_variable_definition <- function(definition, envir = parent.frame()) {
    def <- rlang::enexpr(definition) |> inside()

    if (rlang::is_symbol(def)) {
        name <- def |> format()
        sets <- list(scalar = "")
        return(list(name = name, sets = sets))

    } else if (def[[1]] == quote(`[`)) {
        name <- def[[2]] |> format()
        sets_exprs <- as.list(def[3:length(def)])

        sets_names <- rlang::names2(sets_exprs)
        unnamed <- sets_names == ""
        sets_names[unnamed] <- sets_exprs[unnamed] |> sapply(format)

        sets <- sets_exprs |>
            lapply(eval, envir = envir) |>
            rlang::set_names(sets_names)

        return(list(name = name, sets = sets))
    }

    abort("Failed to parse variable.", call = parent.frame())
}
interpret_bound <- function(bound, default) {
    if (length(bound) > 1L) {
        abort("Lower and upper bounds must be numeric scalars.", call = parent.frame())
    }
    if (length(bound) == 0L || is.na(bound)) {
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

    abort("attempt to recycle variable of length {length(x)} to length {n}")
}
recycle_const <- function(x, n) {
    if (length(x) == n) {
        return(c(x))
    } else if (length(x) == 1L) {
        return(rep(x, n))
    }

    abort("attempt to recycle array of length {length(x)} to length {n}")
}

