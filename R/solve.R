
#' Solve a Linear Problem
#'
#' `lp_solve()` computes the optimal solution of an [lp_problem()], whereas
#' `lp_find_feasible()` returns an arbitrary feasible solution.
#'
#' @param .problem An [lp_problem()].
#' @param solver String specifying the solver to use.
#' If missing, then the default solver returned by [ROI::ROI_options()] is used.
#' @param ... Control arguments to be passed on to the solver.
#' @param start Start value of variables, for nonlinear solvers. One of:
#' - Named list of variables with their respective values.
#' If a variable is missing, it is set to `pmax(0, lower)`.
#' - An `lp_solution` object as returned by [lp_solve()] or [lp_find_feasible()].
#' - A vector containing the values of each variable, one after another.
#' @param binary_as_logical Boolean. If `FALSE` (the default), binary variables
#' are returned as `{0, 1}`. If `TRUE`, binary variables are returned as logical `{FALSE, TRUE}`.
#'
#' @returns A list with the following fields:
#' - `$objective` : Scalar, value of the objective function at optimum.
#' - `$variables` : List of arrays, values of the variables at optimum.
#' - `$aliases` : List of arrays, values of the aliases at optimum.
#' - `$variables_vec` : Numeric vector, values of the variables at optimum.
#' - `$status` : Status as returned by [ROI::ROI_solve()].
#' - `$message` : Message as returned by [ROI::ROI_solve()],
#' - `$op` : Optimization Problem `OP`, as returned by [ROI::as.OP()].
#'
#' @seealso [as.OP.lp_problem()], [pretty_solution()].
#' @export
#'
#' @example inst/examples/example_solve.R
lp_solve <- function(.problem, solver, ..., start, binary_as_logical = FALSE) {
    check_problem(.problem)
    op <- as.OP.lp_problem(.problem)
    applicable <- ROI::ROI_applicable_solvers(op)
    
    if (length(applicable) == 0L) {
        roi_url <- "https://roi.r-forge.r-project.org/installation.html#ROI_plug-ins"
        cli_abort(c(
            "No applicable solvers loaded.",
            ">" = "Use `library(ROI)` to load all installed solvers.",
            ">" = "Use `library(ROI.plugin.<solver>)` to load a specific solver.",
            "i" = "See {.url {roi_url}} for instructions on how to install each solver."
        ))
    }
    
    control <- rlang::dots_list(...)
    
    if (!missing(start)) {
        control$start <- variables_to_vec(
            start, 
            .problem, 
            call = environment(), 
            field = "start"
        )
    }
    
    roi_sol <- ROI_solve(
        op,
        solver = solver,
        control = control
    )
    
    sol <- pretty_solution(
        .problem,
        solution = roi_sol,
        binary_as_logical = binary_as_logical
    )
    
    sol$op <- op
    sol$roi_solution <- roi_sol
    return(sol)
}

#' @rdname lp_solve
#' @export
lp_find_feasible <- function(.problem, binary_as_logical = FALSE, ...) {
    check_problem(.problem)
    
    .problem |>
        lp_minimize(0) |>
        lp_solve(binary_as_logical = binary_as_logical, ...)
}

# Steps -------------------

clear_lpsugar_classes <- function(x) {
    lpsc <- class(x) |> startsWith("lp_")
    class(x) <- class(x)[!lpsc]
    attr(x, "lpsugar_attributes") <- NULL
    return(x)
}

#' @importFrom ROI as.objective
#' @export
as.objective.lp_objective <- function(x) {
    clear_lpsugar_classes(x)
}

#' @importFrom ROI as.constraint
#' @export
as.constraint.lp_constraint <- function(x) {
    clear_lpsugar_classes(x)
}

#' @importFrom ROI as.OP
#' @export
ROI::as.OP

#' @importFrom ROI ROI_solve
#' @export
ROI::ROI_solve

#' Create a [ROI::OP()] Object.
#'
#' Convert an [lp_problem()] object to a [ROI::OP()] object.
#' Used internally in [lp_solve()].
#'
#' @param x An [lp_problem()].
#' @returns An `OP` object as returned from [ROI::OP()].
#'
#' @seealso [pretty_solution()] to prettify the solution returned by [ROI::ROI_solve()].
#' @export
#' @example inst/examples/example_solve_steps.R
as.OP.lp_problem <- function(x) {
    check_problem(x, field_name = "problem")
    
    # No variables
    if (ncol(x) == 0L) {
        cli_abort(
            "Problem has no variables. Define them with `lp_variable()`.",
            class = "lpsugar_error_no_variables_defined"
        )
    }

    if (!rlang::is_scalar_logical(x$maximum) || is.na(x$maximum)) {
        cli_abort(
            c("Must define an objective function with `lp_minimize()` or `lp_maximize()`.",
              "i" = paste(
                  "If you wish to find any feasible solution, use `lp_find_feasible()`",
                  "or set the objective function to 0 with `lp_minimize(0)`",
                  sep = "\n"
              )), 
            class = "lpsugar_error_no_objective"
        )
    }
    
    objective <- as.objective.lp_objective(x$objective)
    constraints <- as.constraint.lp_constraint(x$constraints)
    
    types <- character(ncol(x))
    lower <- numeric(ncol(x))
    upper <- numeric(ncol(x))
    
    for (v in x$variables) {
        types[v$ind] <- v$type
        lower[v$ind] <- v$lower
        upper[v$ind] <- v$upper
    }
    
    # Bound indices and bounds
    li <- which(lower != 0)
    ui <- which(is.finite(upper))
    lb <- lower[li]
    ub <- upper[ui]
    
    bounds <- ROI::V_bound(
        li = li, ui = ui,
        lb = lb, ub = ub,
        nobj = ncol(x)
    )
    
    ROI::OP(
        objective = objective,
        maximum = x$maximum,
        types = types,
        bounds = bounds,
        constraints = constraints
    )
}

#' Prettify the Solution of a Model.
#'
#' Takes a problem and its solution and prettifies the solution. Used internally
#' in [lp_solve()].
#'
#' @param problem An [lp_problem()].
#' @param solution A list as returned by [ROI::ROI_solve()].
#'
#' @seealso [lp_solve()] for the standard way to solve a problem.
#'
#' [as.OP.lp_problem()] to convert an `lp_problem` to an Optimization Problem `(OP)` object
#' from package `ROI`.
#'
#' @inherit lp_solve
#' @export
#'
#' @example inst/examples/example_solve_steps.R
pretty_solution <- function(problem, solution, binary_as_logical = FALSE) {
    check_problem(problem, field_name = "problem")
    check_roi_solution(solution)
    
    if (length(solution$objval) == 0) {
        solution$objval <- NA_real_
    }
    
    if (length(solution$solution) == 0) {
        out <- list(
            objective = NA_real_,
            variables_vec = rep(NA_real_, ncol(problem)),
            status = solution$status,
            message = solution$message
        ) |> structure(class = "lp_solution")
        
        return(out)
    }
    
    vars <- variables_to_list(
        solution$solution, 
        problem = problem, 
        miss_error = FALSE,
        binary_as_logical = binary_as_logical
    )
    
    als <- compute_aliases(problem, solution$solution)
    objective_info <- lpsugar_attributes(problem$objective)
    objective <- solution$objval + objective_info$A
    
    list(
        objective = objective,
        variables = vars,
        aliases = als,
        variables_vec = solution$solution,
        status = solution$status,
        message = solution$message
    ) |> structure(class = "lp_solution")
}

# User Utils ----------------------------

#' @importFrom ROI ROI_available_solvers
#' @export
ROI_available_solvers.lp_problem <- function(x, method = getOption("download.file.method")) {
    ROI_available_solvers(as.OP(x), method = method)
}

#' List Applicable and Available Solvers
#' 
#' Show which solvers can solve a problem. Applicable solvers must be installed and loaded,
#' whereas Available solvers needn't be installed.
#'
#' @param problem An [lp_problem()] or a [ROI::OP()].
#'
#' @details
#' - `lpsugar_applicable_solvers` returns a character vector of solver names, which
#' can be used in `lp_solve(solver = _)`. It lists solvers which:
#'   - Can solve the `problem`.
#'   - Are installed and have been loaded with `library(ROI)` or `library(ROI.plugin.<solver>)`.
#' 
#' - `lpsugar_available_solvers` returns a `data.frame` with information on the solvers.
#' It lists solvers which:
#'   - Can solve the `problem`.
#'   - Do not need to be installed.
#' 
#' Note since Nonlinear Solvers are also applicable to Linear and Quadratic problems, 
#' they will also be listed.
#' 
#' @returns 
#' - `lpsugar_applicable_solvers` returns a character vector with the solver names.
#' - `lpsugar_available_solvers` returns a `data.frame` with information on the solvers.
#' @seealso [ROI::ROI_applicable_solvers()], [ROI::ROI_available_solvers()].
#' 
#' @export
#'
#' @example inst/examples/example_applicable_solvers.R
lpsugar_applicable_solvers <- function(problem) {
    if (inherits(problem, "lp_problem")) {
        op <- as.OP(problem)
        ROI::ROI_applicable_solvers(op)
    } 
    else if (inherits(problem, "OP")) {
        ROI::ROI_applicable_solvers(problem)
    } 
    else {
        cli_abort("`problem` must be an `lp_problem` or an `OP` object.")
    }
}

#' @rdname lpsugar_applicable_solvers
#' @export
lpsugar_available_solvers <- function(problem) {
    ROI_available_solvers(problem)
}

#' @importFrom ROI solution
#' @export
solution.lp_solution <- function(
        x, 
        type = c("primal", "dual", "aux", "psd", "msg", "objval", "status", "status_code"),
        force = FALSE,
        ...
) {
    ROI::solution(
        x$roi_solution, 
        type = type, 
        force = force, 
        ...
    )
}

# Methods -------------------------------

#' @export
print.lp_solution <- function(x, ...) {
    if (!is.na(x$objective)) {
        print_field(x, "variables")
        
        if (length(x$aliases) > 0L) {
            print_field(x, "aliases")
        }
        
        print_field(x, "objective")
        cat("\n")
    }
    
    print_field_name("status")
    
    if (cli::is_utf8_output()) {
        tick <- cli::symbol$tick
        cross <- cli::symbol$checkbox_circle_on
    } 
    else {
        tick <- "[v]"
        cross <- "[x]"
    }
    
    if (x$status$code == 0) {
        cat("Optimal Solution Found", tick, "\n\n")
    } 
    else {
        cat("No Optimal Solution Found", cross, "\n\n")
        print(x$status)
    }
}
