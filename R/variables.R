
#' @export
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

    # Index array of variable.
    # Indicates which objective coefficients correspond to this variable.
    ind <- array(dim = lengths(sets), dimnames = sets) |> robust_index()
    ind[] <- 1:length(ind) + .problem$.nvar
    .problem$.nvar <- max(ind)

    .problem$.varnames <- c(
        .problem$.varnames,
        name_variable(name, sets)
    )

    type <- if (binary)
        "binary"
    else if (integer)
        "integer"
    else
        "real"


    new_variable <- list(
        name = name,
        sets = sets,
        bound = c(Lower = lower, Upper = upper),

        type = type,
        integer = integer,
        binary = binary,

        ind = ind,
        raw = TRUE,
        indexable = TRUE

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

        x$add <- numeric(total_vars)
        names(x$add) <- varnames

        x$coef <- matrix(0, nrow = length(x), ncol = total_vars) |> robust_index()
        x$coef[, x$ind] <- diag(length(x))
        colnames(x$coef) <- varnames

        # v$selected <- numeric(total_vars)
        # v$selected[v$ind] <- TRUE

        .problem$variables[[i]] <- x
    }

    .problem
}
update_objective <- function(.problem) {
    total_vars <- .problem$.nvar
    objective_len <- length(.problem$objective)

    .problem$objective <- c(
        .problem$objective,
        numeric(total_vars - objective_len)
    )

    names(.problem$objective) <- .problem$.varnames
    .problem
}

# Methods --------------------

#' @export
print.lp_variable <- function(x, ...) {
    if (!x$raw) {
        print(x$coef)
        return(x)
    }

    cat("Linear Programming Variable '", x$name, "'", sep = "")

    if (x$binary)
        cat(" <binary>")
    else if (x$integer)
        cat(" <integer>")

    if (length(x$ind) > 1L)
        cat("\nWith sets: [", paste(names(x$sets), collapse = ", "), "]")

    if (all(x$bound != c(-Inf, +Inf))) {
        cat("\n")
        cat(x$bound[1L], "<=", x$name, "<=", x$bound[2L])
    } else if (x$bound[1L] != -Inf) {
        cat("\n", x$name, " >= ", x$bound[1L], sep = "")
    } else if (x$bound[2L] != +Inf) {
        cat("\n", x$name, " <= ", x$bound[2L], sep = "")
    }
    cat("\n")
    return(x)
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


# Indexing ----------------------

#' @export
`[.lp_variable` <- function(x, ...) {
    if (!x$indexable) {
        abort("Cannot index this result.")
    }

    old_ind <- x$ind
    x$ind <- x$ind[...]
    x$raw <- FALSE

    keep <- is.element(old_ind, x$ind)
    x$coef <- x$coef[keep, ]
    x$add <- x$add[keep]

    x
}
#' @export
`[[.lp_variable` <- function(x, ...) {
    abort("`lp_variable`s don't support double indexing `{x$name}[[i]]`. Use `{x$name}[i]` instead")
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

    abort("Failed to parse variable `{format(def)}`.")
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
