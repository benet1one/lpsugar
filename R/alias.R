
#' Define an Alias or Implicit Variable (IMPVAR)
#'
#' @param .problem An [lp_problem()].
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
lp_alias <- function(.problem, ...) {
    dots <- rlang::enquos(...)
    nams <- rlang::names2(dots)

    for (d in seq_along(dots)) {
        data <- data_mask(.problem)
        .problem <- lp_alias_internal(.problem, dots[[d]], nams[d], data)
    }

    return(.problem)
}

lp_alias_internal <- function(.problem, quosure, name, data) {
    fs <- for_split(quosure, evaluate = FALSE)
    is_fs <- is_for_split(fs)
    is_named <- name != ""

    if (is_named + is_fs != 1L) {
        abort("Either name the alias or use `for` notation. See ?lp_alias for details.",
              call = parent.frame())
    }

    new_alias <- if (is_named) {
        lp_alias_named(quosure, name, data)
    } else {
        lp_alias_fs(fs, data)
    }

    if (new_alias$name %in% names(.problem$variables)) {
        abort("cannot override variable `{new_alias$name}`", call = parent.frame())
    } else if (new_alias$name %in% names(.problem$aliases)) {
        inform("overriding alias `{new_alias$name}`", call = parent.frame())
    }

    .problem$aliases[[new_alias$name]] <- new_alias$value
    return(.problem)
}

lp_alias_named <- function(quosure, name, data) {
    value <- rlang::eval_tidy(quosure, data = data)

    if (!is_lp_variable(value)) {
        abort("did not evaluate to a variable", call = quosure)
    }

    list(name = name, value = value)
}

lp_alias_fs <- function(fs, data) {
    abort("not yet supported", call = parent.frame())
}

# Alias --------------------

#' @rdname lp_alias
#' @export
lp_implicit_variable <- lp_alias
#' @rdname lp_alias
#' @export
lp_impvar <- lp_alias
