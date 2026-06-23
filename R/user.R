
#' Set Dimensions and Names for Parameters
#'
#' Return a named vector or matrix.
#'
#' @param .x Vector or matrix of any type.
#' @param ... Dimnames. If you want to return a vector, it needs one element;
#' if you want to return a matrix it needs two elements.
#' @param byrow Boolean, true by default. Only used if `.x` is a vector and `...` has two elements.
#'
#' @returns A named vector or matrix.
#' @export
#'
#' @examples
#' my_set <- letters[1:3]
#' my_parameter <- c(2, 6, 3) |> parameter(my_set)
#' my_parameter
#'
#' rows <- letters[1:2]
#' cols <- LETTERS[1:3]
#' my_matrix <- c(
#'     1, 2, 3,
#'     4, 5, 6
#' ) |> parameter(rows, cols)
#' my_matrix
#'
#' # Also works if .x is already a matrix.
#' mat <- matrix(1:6, nrow = 3, ncol = 2)
#' mat |> parameter(r = letters[1:3], c = LETTERS[1:2])
parameter <- function(.x, ..., byrow = TRUE) {
    dots <- rlang::dots_list(..., .named = TRUE, .ignore_empty = "none")
    
    if (length(dots) == 1L) {
        warn_changed_args(byrow = TRUE)
        parameter_vector(.x, dots)
    } 
    else if (length(dots) == 2L) {
        parameter_matrix(.x, dots, byrow = byrow)
    } 
    else if (length(dots) == 0L) {
        cli_abort("`...` cannot be empty.")
    } 
    else {
        cli_abort("Only vectors and matrices are supported.")
    }
}

parameter_vector <- function(.x, dots) {
    nd <- sum(dim2(.x) > 1L)
    
    if (nd > 1L) {
        cli_abort(
            "`.x` has two or more dimensions but only one element in `...`",
            call = parent.frame()
        )
    }
    
    n <- length(dots[[1]])
    
    if (length(.x) > 1L && length(.x) != n) {
        cli_abort(
            c("Lengths do not match.",
              "x" = "`.x` is length {length(.x)}",
              "x" = "`{names(dots)}` is length {n}"),
            call = parent.frame()
        )
    }
    
    array(.x, dim = length(dots[[1]]), dimnames = dimnames_non_numeric(dots))
}

parameter_matrix <- function(.x, dots, byrow = TRUE) {
    n <- lengths(dots)[1]
    m <- lengths(dots)[2]
    
    if (ndim(.x) == 1L) {
        if (length(.x) != 1L && length(.x) != n*m) {
            cli_abort(
                c("Lengths do not match.",
                  "x" = "`.x` is length {length(.x)}",
                  "x" = "It should be length {n*m} = {n} x {m}"),
                call = parent.frame()
            )
        }
        
        return(matrix(
            .x, nrow = n, ncol = m,
            byrow = byrow, dimnames = dimnames_non_numeric(dots)
        ))
    } 
    else if (ndim(.x) == 2L) {
        if (any(dim(.x) != lengths(dots))) {
            cli_abort(
                c("Dimensions do not match.",
                  "x" = "`.x`  has dimensions ({nrow(.x)}, {ncol(.x)})",
                  "x" = "`...` has dimensions ({n}, {m})"),
                call = parent.frame()
            )
        }
        
        warn_changed_args(byrow = TRUE)
        dimnames(.x) <- dimnames_non_numeric(dots)
        return(.x)
    } 
    else {
        cli_abort(
            "`.x` has {ndim(.x)} dimensions. Only vectors and matrices are supported.",
            call = parent.frame()
        )
    }
}


#' Compute a Summary of a Solution or Point
#'
#' Check feasibility, constraint saturation, calculate objective value and aliases.
#'
#' @param problem An [lp_problem()].
#' @param solution One of:
#' - Named list of variables with their respective values.
#' - An `lp_solution` object as returned by [lp_solve()] or [lp_find_feasible()].
#' - Vector containing the values of each variable, one after another.
#' @param tol Tolerance to use for constraint and bound satisfaction.
#'
#' @returns A named list with the computed statistics.
#' @export
#'
#' @example inst/examples/example_solution_summary.R
solution_summary <- function(problem, solution, tol = 2e-6) {
    solution <- variables_to_vec(solution, problem, call = environment(), field = "solution")
    aliases <- compute_aliases(problem, solution)
    constraints <- constraint_summary(problem, solution, tol = tol)
    bounds <- bound_summary(problem, solution, tol = tol)
    
    list(
        aliases = aliases,
        constraints = constraints,
        bounds = bounds,
        feasible = all(constraints$satisfied) && all(bounds$satisfied),
        objective = compute_objective(problem, solution)
    )
}

#' @rdname solution_summary
#' @export
constraint_summary <- function(problem, solution, tol = 2e-6) {
    solution <- variables_to_vec(solution, problem, call = environment(), field = "solution")
    con <- problem$constraints
    
    quadratic_part <- numeric(nrow(con))
    q_ind <- which(lengths(con$q_lhs) > 0L)
    
    for (i in q_ind) {
        q <- con$q_lhs[[i]]
        row_sol <- t(solution)
        col_sol <- t(row_sol)
        quadratic_part[i] <- 0.5 * row_sol %*% q %*% col_sol
    }
    
    linear_part <- con$lhs %*% solution
    lhs <- quadratic_part + linear_part
    dir <- con$dir
    rhs <- con$rhs[, 1]
    diff <- rhs - lhs
    
    less_than <- lhs <= rhs + tol
    greater_than <- lhs >= rhs - tol
    equal_to <- less_than & greater_than
    
    satisfied <- logical(length(dir))
    satisfied[dir == "<="] <- less_than[dir == "<="]
    satisfied[dir == ">="] <- greater_than[dir == ">="]
    satisfied[dir == "=="] <- equal_to[dir == "=="]
    
    feasible <- all(satisfied)
    saturated <- diff > -tol  &  diff < +tol
    saturated[!satisfied] <- NA
    
    df <- data.frame(
        name = con$name,
        fullname = rownames(con),
        lhs = lhs,
        dir = dir,
        rhs = rhs,
        satisfied = satisfied,
        saturated = saturated
    )
    
    rownames(df) <- NULL
    df
}

#' @rdname solution_summary
#' @export
bound_summary <- function(problem, solution, tol = 2e-6) {
    solution <- variables_to_vec(solution, problem, call = environment(), field = "solution")
    
    lower <- numeric(ncol(problem))
    upper <- numeric(ncol(problem))
    
    for (x in problem$variables) {
        lower[x$ind] <- x$lower
        upper[x$ind] <- x$upper
    }
    
    satisfied <- (solution >= lower - tol) & (solution <= upper + tol)
    saturated <- (solution <= lower + tol) | (solution >= upper - tol)
    saturated[!satisfied] <- NA
    
    df <- data.frame(
        variable = attr(problem, "varnames"),
        lower = lower,
        value = solution,
        upper = upper,
        satisfied = satisfied,
        saturated = saturated
    )
    
    rownames(df) <- NULL
    df
}

#' @rdname solution_summary
#' @export
compute_objective <- function(problem, solution) {
    solution <- variables_to_vec(solution, problem, call = environment(), field = "solution")
    
    if (problem$objective$type == "nonlinear") {
        return(problem$objective$fun(solution))
    }
    
    coef <- problem$objective$coef
    add <- problem$objective$add
    out <- crossprod(coef, solution) + add
    
    if (is_quadratic(problem$objective)) {
        row_sol <- t(solution)
        col_sol <- t(row_sol)
        q <- problem$objective$q_coef
        
        q_out <- 0.5 * row_sol %*% q %*% col_sol
        out <- out + q_out
    }
    
    out[1]
}

#' @rdname solution_summary
#' @export
compute_aliases <- function(problem, solution) {
    solution <- variables_to_vec(
        solution, 
        problem, 
        miss_error = FALSE, 
        call = environment(),
        field = "solution"
    )
    
    purrr::map(problem$aliases, function(a) {
        mat <- array(unclass(a$coef), dim = dim(a$coef))
        add <- unclass(a$add)
        out <- mat %*% solution + add
        
        if (is_quadratic(a)) {
            row_sol <- t(solution)
            col_sol <- t(row_sol)
            
            q_out <- purrr::map_dbl(a$q_coef, function(qi) {
                0.5 * row_sol %*% qi %*% col_sol
            })
            
            out <- out + q_out
        }
        
        if (length(out) == 1L) {
            unname(out[1])
        } 
        else {
            array(out, dim = dim2(a), dimnames = dimnames(a))
        }
    })
}
