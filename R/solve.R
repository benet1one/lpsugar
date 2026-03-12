
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
#' description of the solver. Some common status are:
#' - `0 | optimal solution found`
#' - `2 | the model is infeasible`
#' - `3 | the model is unbounded`
#' For a full list of status and their meaning see [lpSolveAPI::solve.lpExtPtr()].
#' @param ... Control parameters passed to [lpSolveAPI::lp.control()]. For a full list of
#' options see [lpSolveAPI::lp.control.options()].
#'
#' @returns
#' @export
#'
#' @examples
lp_solve <- function(.problem, binary_as_logical = FALSE, unbound_as_inf = TRUE,
                     report_status = FALSE, ...) {
    model <- make_model(.problem, ...)
    solution_raw <- solve_model(model, report_status = report_status)

    pretty_solution(
        .problem,
        solution = solution_raw,
        binary_as_logical = binary_as_logical,
        unbound_as_inf = unbound_as_inf
    )
}

# Steps -------------------

make_model <- function(problem, ...) {
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
        verbose = "full" # for now
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

    list(
        objective = lpSolveAPI::get.objective(model),
        variables = lpSolveAPI::get.variables(model),
        status_number = status_number,
        status_description = status_description,
        pointer = model
    )
}

pretty_solution <- function(problem, solution = solve_model(problem),
                            binary_as_logical = FALSE, unbound_as_inf = TRUE) {
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
