
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
#' @seealso [lp_alias_manual()]
#'
#' @example inst/examples/example_alias.R
lp_alias <- function(.problem, ...) {
    check_problem(.problem)
    dots <- rlang::enquos(...)
    nams <- rlang::names2(dots)
    
    if (ncol(.problem) == 0L) {
        cli_abort(
            "Must define variables before aliases.", 
            class = "lpsugar_error_alias_before_variables"
        )
    }
    
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

# New Aliases ---------------

#' Manually Define an Alias by Assigning its Values
#' 
#' This function does the same as [lp_alias()], but gives more freedom. 
#' It provides a better syntax for defining aliases that cannot be defined
#' in a single line of code.
#'
#' @inheritParams lp_variable
#' 
#' @param expression Code to assign values to the alias. The values must be
#' numeric or `<lp_variable>`. See examples.
#' 
#' @returns The `.problem` with the added alias in `$aliases`.
#' @export
#'
#' @seealso [lp_alias()]
#'
#' @examples
lp_alias_manual <- function(.problem, definition, expression) {
    check_problem(.problem)
    
    if (ncol(.problem) == 0L) {
        cli_abort(
            "Must define variables before aliases.", 
            class = "lpsugar_error_alias_before_variables"
        )
    }
    
    if (missing(definition)) {
        cli_abort("Argument `definition` is missing, with no default.")
    }
    
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
    else if (name %in% names(.problem$aliases)) {
        cli_inform("Overriding impvar `{name}`.", call = parent.frame())
    }
    
    fixed_at <- rep(FALSE, prod(lengths(sets)))
    
    ind <- variable_indices(
        old_n = 0L, 
        definition = def,
        fixed_at = fixed_at
    )

    A <- matrix(NA_real_, nrow = length(ind), ncol = 1L) |> 
        robust_index()
    
    L <- matrix(0, nrow = length(ind), ncol = ncol(.problem)) |> 
        robust_index()
    
    colnames(L) <- variable.names(.problem)

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
    
    alias <- rlang::eval_tidy(expr, env = env, data = data_mask(.problem))
    .problem$aliases[[name]] <- alias

    if (all(is.na(alias$A))) {
        cli_abort(
            c("No values defined for `{name}`.",
              "i" = "Did you accidentally use `==` instead of `<-` or `=`?"),
            class = "lpsugar_error_alias_undefined"
        )
    }
    else if (anyNA(alias$A)) {
        i <- unclass(alias$ind)
        i[] <- c(is.na(alias$A))
        first_miss <- which(i == 1, arr.ind = TRUE)[1, ]
        first_miss <- format_dim(dim = first_miss)
        
        cli_abort(
            c("Alias `{name}` is not fully defined.",
              "x" = "{sum(i)} unassigned values.",
              "x" = "First unassigned value at {first_miss}."),
            class = "lpsugar_error_alias_undefined"
        )
    }

    return(.problem)
}

# Alias --------------------

#' @rdname lp_alias
#' @export
lp_impvar <- lp_alias

#' @rdname lp_alias
#' @export
lp_implicit_variable <- lp_alias

#' @rdname lp_alias_manual
#' @export
lp_impvar_manual <- lp_alias_manual
