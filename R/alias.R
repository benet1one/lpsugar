
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
        cli_abort("Aliases must be named.", class = "lpsugar_error_unnamed_alias")
    }
    
    for (d in seq_along(dots)) {
        data <- data_mask(.problem)
        .problem <- lp_alias_internal(.problem, dots[[d]], nams[d], data)
    }
    
    return(.problem)
}

lp_alias_internal <- function(.problem, quosure, name, data) {
    if (name %in% names(.problem$variables)) {
        cli_abort(
            "Cannot override variable `{name}`.", 
            class = "lpsugar_error_alias_override_variable",
            call = parent.frame()
        )
    } 
    else if (name %in% names(.problem$aliases)) {
        cli_inform("Overriding alias `{name}`.", call = parent.frame())
    }
    
    value <- rlang::eval_tidy(quosure, data = data)
    
    if (is_nonlinear(value)) {
        cli_abort(
            "Aliases cannot be `nonlinear()`",
            class = "lpsugar_error_nonlinear_alias",
            call = parent.frame()
        )
    }
    if (!is_lp_variable(value)) {
        cli_abort(
            "Alias `{name}` did not evaluate to a variable.", 
            class = "lpsugar_error_alias_not_a_variable",
            call = parent.frame()
        )
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



# New Impvars ---------------

#' Define an Alias or Implicit Variable (IMPVAR)
#'
#' @param .problem An [lp_problem()].
#' @param definition 
#' @param expression 
#' @param default 
#'
#' @returns
#' @export
#'
#' @examples
lp_alias_2 <- function(.problem, definition, expression, default = 0) {
    check_problem(.problem)
    
    if (missing(definition)) {
        cli_abort("Argument `definition` is missing, with no default.")
    }
    
    stopifnot(is.numeric(default) && length(default) == 1)
    
    def <- parse_variable_definition({{ definition }})
    name <- def$name
    sets <- def$sets
    
    if (name %in% names(.problem$variables)) {
        cli_abort(
            "Cannot override variable `{name}`.", 
            class = "lpsugar_error_impvar_override_variable",
            call = parent.frame()
        )
    } 
    else if (name %in% names(.problem$impvars)) {
        cli_inform("Overriding impvar `{name}`.", call = parent.frame())
    }
    
    fixed_at <- rep(FALSE, prod(lengths(sets)))
    fixed_values <- NA
    
    ind <- variable_indices(
        old_n = 0L, 
        definition = def,
        fixed_at = fixed_at
    )
    
    A <- new_A_coef(
        ind = ind, 
        fixed_at = fixed_at, 
        fixed_values = fixed_values
    )
    
    L <- new_L_coef(
        ind = ind,
        ncol = ncol(.problem),
        colnames = variable.names(.problem),
        fixed_at = fixed_at
    )
    
    L[] <- 0
    A[] <- default
    
    variable <- list(
        binary = FALSE,
        ind = ind,
        L = L,
        A = A
    ) |> structure(class = c("transformed_lp_variable", "lp_variable"))
    
    quo <- rlang::enquo(expression)
    expr <- rlang::get_expr(quo)
    
    expr <- rlang::expr({
        !!expr
        !!rlang::sym(name)
    })
    
    env <- rlang::get_env(quo)
    env[[name]] <- variable
    
    variable_new <- rlang::eval_tidy(expr, env = env, data = data_mask(.problem))
    .problem$impvars[[name]] <- variable_new

    return(.problem)
}
