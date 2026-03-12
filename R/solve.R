
#' Solve a Linear Problem
#'
#' Attempt to compute the optimal solution of an [lp_problem()].
#'
#' @param .problem An [lp_problem()].
#' @param binary_as_logical Boolean. If `FALSE` (the default), binary variables
#' are returned as `{0, 1}`. If `TRUE`, binary variables are returned as logical `{FALSE, TRUE}`.
#' @param unbound_as_inf Boolean, whether to replace very large numbers with `+Inf` and
#' very small numbers with `-Inf`.
#' @param report_status Boolean, whether to emit a message indicating the status number and
#' description of the solver. For a full list of status and their meaning
#' see [lpSolveAPI::solve.lpExtPtr()].Some common status are:
#' - `0 | optimal solution found`
#' - `2 | the model is infeasible`
#' - `3 | the model is unbounded`
#'
#' @param ... Control parameters passed to [lpSolveAPI::lp.control()]. For a full list of
#' options see [lpSolveAPI::lp.control.options()].
#' @param verbose String indicating the severity of messages reported by `lp_solve`.
#' - `"neutral"` : No reporting.
#' - `"critical"` : Only critical messages are reported. Hard errors like instability, out of memory, etc.
#' - `"severe"` :	Only severe messages are reported. Errors.
#' - `"important"` : Only important messages are reported. Warnings and Errors.
#' - `"normal"` :	Normal messages are reported.
#' - `"detailed"` : Detailed messages are reported. Like model size, continuing B&B improvements, etc.
#' - `"full"` : All messages are reported. Useful for debugging purposes and small models.
#'
#' @returns A list with the following fields:
#' - `$objective` : Numeric scalar, value of the objective function at the optimal.
#' - `$variables` : Named list with the optimal value for each variable.
#' - `$aliases` : Named list with the value of each [lp_alias()].
#' - `$status_number` : Integer indicating the status of the solver.
#' - `$status_description` : String describing the status of the solver.
#' - `$pointer` : Pointer to the `lpSolveAPI` model, class `lpExtPtr`.
#' @export
#'
#' @examples
lp_solve <- function(.problem, binary_as_logical = FALSE, unbound_as_inf = TRUE,
                     report_status = FALSE, verbose = "severe", ...) {
    check_problem(.problem)
    model <- make_model(.problem, verbose = verbose, ...)
    solution_raw <- solve_model(model, report_status = report_status)

    pretty_solution(
        .problem,
        solution = solution_raw,
        binary_as_logical = binary_as_logical,
        unbound_as_inf = unbound_as_inf
    )
}

# Steps -------------------

#' Make a Linear Program Model
#'
#' Create an `lpExtPtr` pointer from the `lpSolveAPI` package.
#' Used internally in [lp_solve()].
#'
#' @inheritParams lp_solve
#'
#' @returns An `lpExtPtr` pointer from the `lpSolveAPI` package.
#' @export
#'
#' @examples
make_model <- function(problem, verbose = "severe", ...) {
    check_problem(problem)

    if (problem$.nvar == 0L) {
        abort("Problem has no variables. Define them with `lp_variable()`.")
    }

    dir <- if (problem$objective$direction == "minimize") {
        "min"
    } else if (problem$objective$direction == "maximize") {
        "max"
    } else {
        abort("`$objective$direction` should be either 'minimize' or 'maximize'.")
    }

    ptr <- lpSolveAPI::make.lp(
        nrow = 0,
        ncol = problem$.nvar,
        verbose = verbose
    )

    lpSolveAPI::set.objfn(ptr, problem$objective$coef)
    lpSolveAPI::lp.control(ptr, sense = dir, ...)

    for (x in problem$variables) {
        lpSolveAPI::set.type(
            ptr,
            columns = x$ind,
            type = x$type
        )
        lpSolveAPI::set.bounds(
            ptr,
            columns = x$ind,
            lower = rep_len(x$lower, length(x)),
            upper = rep_len(x$upper, length(x))
        )
    }

    con <- problem$constraints
    con$dir <- ifelse(con$dir == "==", yes = "=", no = con$dir)

    for (i in seq_along(con)) {
        lpSolveAPI::add.constraint(
            ptr,
            xt = con$lhs[i, ],
            type = con$dir[i],
            rhs = con$rhs[i]
        )
    }

    # rownames(ptr) <- problem$constraints$name
    colnames(ptr) <- problem$.varnames
    ptr
}

#' Solve a Model
#'
#' Find the optimal solution of a model created with [make_model()] or [lpSolveAPI::make.lp()].
#' Used internally in [lp_solve()].
#'
#' @param model A pointer of class `lpExtPtr` created with [make_model()] or [lpSolveAPI::make.lp()].
#' @inheritParams lp_solve
#'
#' @section Value:
#'  A list with fields:
#' - `$objective` : Numeric scalar, value of the objective function at the optimum.
#' NOTE: This value does not include the addend `(problem$objective$add)`.
#' - `$variables` : Numeric vector with the value of the variables at the optimum.
#' - `$status_number` : Integer indicating the status of the solver.
#' - `$status_description` : String describing the status of the solver.
#' - `$pointer` : Pointer to the `lpSolveAPI` model, class `lpExtPtr`.
#' @export
#'
#' @examples
solve_model <- function(model, report_status = FALSE) {
    if (!inherits(model, "lpExtPtr")) {
        abort("Model must be an `lpExtPtr` object created with `make_model()`.")
    }

    status_number <- lpSolveAPI::solve.lpExtPtr(model)
    status_description <- switch(
        as.character(status_number),
        "0" = "optimal solution found",
        "1" = "the model is sub-optimal",
        "2" = "the model is infeasible",
        "3" = "the model is unbounded",
        "4" = "the model is degenerate",
        "5" = "numerical failure encountered",
        "6" = "process aborted",
        "7" = "timeout",
        "9" = "the model was solved by presolve",
        "10" = "the branch and bound routine failed",
        "11" = "the branch and bound was stopped because of a break-at-first or break-at-value",
        "12" = "a feasible branch and bound solution was found",
        "13" = "no feasible branch and bound solution was found",
        "undocumented status"
    )

    if (report_status) {
        inform("Status = {status_number} | {status_description}.")
    }

    objective <- lpSolveAPI::get.objective(model)
    variables <- lpSolveAPI::get.variables(model) |>
        rlang::set_names(colnames(model))

    list(
        objective = objective,
        variables = variables,
        status_number = status_number,
        status_description = status_description,
        pointer = model
    )
}

#' Prettify the Solution of a Model.
#'
#' Takes a problem and its solution and prettifies the solution. Used internally
#' in [lp_solve()].
#'
#' @param solution A list created with [solve_model()].
#' @inherit lp_solve
#' @export
#'
#' @examples
pretty_solution <- function(problem, solution,
                            binary_as_logical = FALSE, unbound_as_inf = TRUE) {
    check_problem(problem)
    if (unbound_as_inf) {
        solution$objective <- solution$objective |> large_to_infinity()
        solution$variables <- solution$variables |> large_to_infinity()
    }

    vars <- list()

    for (x in problem$variables) {
        vars[[x$name]] <- solution$variables[x$ind]

        if (x$binary && binary_as_logical) {
            vars[[x$name]] <- vars[[x$name]] > 0.5
        }
    }

    als <- list()

    for (a in problem$aliases) {
        als[[a$name]] <- tcrossprod(a$coef, solution$variables) + a$add
        als[[a$name]] <- als[[a$name]] |> as.matrix()
    }

    objective <- solution$objective + problem$objective$add

    list(
        objective = objective,
        variables_vec = solution$variables,
        variables = vars,
        aliases = als,
        status_number = solution$status_number,
        status_description = solution$status_description,
        pointer = solution$pointer
    )
}
