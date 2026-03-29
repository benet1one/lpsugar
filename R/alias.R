
#' Define an Alias or Implicit Variable (IMPVAR)
#'
#' Create a 'fake' variable that can be used in constraints and objective without adding
#' complexity to the problem.
#'
#' @param .problem An [lp_problem()].
#' @param ... Name-value pairs. The name will be the name of the alias. The value must be
#' a linear function of previously defined variables or aliases.
#'
#' @returns The `.problem` with the added `$aliases`.
#' @export
#'
#' @example inst/examples/example_alias.R
lp_alias <- function(.problem, ...) {
    check_problem(.problem)
    dots <- rlang::enquos(...)
    nams <- rlang::names2(dots)

    if (any(nams == "")) {
        abort("Aliases must be named.")
    }

    for (d in seq_along(dots)) {
        data <- data_mask(.problem)
        .problem <- lp_alias_internal(.problem, dots[[d]], nams[d], data)
    }

    return(.problem)
}

lp_alias_internal <- function(.problem, quosure, name, data) {
    if (name %in% names(.problem$variables)) {
        abort("Cannot override variable `{name}`.", call = parent.frame())
    } else if (name %in% names(.problem$aliases)) {
        inform("Overriding alias `{name}`.", call = parent.frame())
    }

    value <- rlang::eval_tidy(quosure, data = data)

    if (!is_lp_variable(value)) {
        abort("Did not evaluate to a variable.", call = quosure)
    }

    .problem$aliases[[name]] <- value
    return(.problem)
}

# Alias --------------------

#' @rdname lp_alias
#' @export
lp_implicit_variable <- lp_alias
#' @rdname lp_alias
#' @export
lp_impvar <- lp_alias
