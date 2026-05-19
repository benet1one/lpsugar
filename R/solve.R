
#' Solve a Linear Problem
#'
#' `lp_solve()` computes the optimal solution of an [lp_problem()], whereas
#' `lp_find_feasible()` returns an arbitrary feasible solution.
#'
#' @param .problem An [lp_problem()].
#' @param solver String specifying the solver to use.
#' If missing, then the default solver returned by [ROI::ROI_options()] is used.
#' @param binary_as_logical Boolean. If `FALSE` (the default), binary variables
#' are returned as `{0, 1}`. If `TRUE`, binary variables are returned as logical `{FALSE, TRUE}`.
#' @param ... Control arguments to be passed on to the solver.
#'
#' @export
#'
#' @example inst/examples/example_solve.R
lp_solve <- function(.problem, solver, binary_as_logical = FALSE, ...) {
    check_problem(.problem)
    model <- make_model(.problem)
    solution_raw <- solve_model(model, solver, ...)

    pretty_solution(
        .problem,
        solution = solution_raw,
        binary_as_logical = binary_as_logical
    )
}

#' @rdname lp_solve
#' @export
lp_find_feasible <- function(.problem, binary_as_logical = FALSE, ...) {
    check_problem(.problem)

    .problem |>
        lp_minimize(0) |>
        lp_solve( binary_as_logical = binary_as_logical, ...)
}

# Steps -------------------

#' @importFrom ROI as.L_objective
#' @export
as.L_objective.lp_problem <- function(x) {
    if (is_quadratic(x$objective)) {
        ROI::Q_objective(
            Q = x$objective$q_coef,
            L = x$objective$coef,
            names = attr(x, "varnames")
        )
    } else {
        ROI::L_objective(
            L = x$objective$coef,
            names = attr(x, "varnames")
        )
    }
}

#' @importFrom ROI as.L_constraint
#' @export
as.L_constraint.lp_problem <- function(x, ...) {
    rlang::check_dots_empty()

    if (length(x$constraints) == 0L) {
        return(ROI::NO_constraint(n_obj = ncol(x)))
    }

    ROI::L_constraint(
        L = x$constraints$lhs,
        dir = c(x$constraints$dir),
        rhs = c(x$constraints$rhs),
        names = attr(x, "varnames")
    )
}

#' Make an Optimization Problem
#'
#' Translate an [lp_problem()] object to a [ROI::OP()] object. Used
#' internally in [lp_solve()].
#'
#' @param problem An [lp_problem()].
#' @returns An `OP` object as returned from [ROI::OP()].
#'
#' @export
#' @example inst/examples/example_solve_steps.R
make_model <- function(problem) {
    check_problem(problem, field_name = "problem")

    # No variables
    if (ncol(problem) == 0L) {
        abort("Problem has no variables. Define them with `lp_variable()`.")
    }

    # Direction
    maximize <- if (problem$objective$direction == "minimize") {
        FALSE
    } else if (problem$objective$direction == "maximize") {
        TRUE
    } else {
        rlang::abort(c(
            "`$objective$direction` should be either 'minimize' or 'maximize'.",
            ">" = "Did you forget to set the objective function?",
            "i" = paste(
                "If you wish to find any feasible solution, use `lp_find_feasible()`",
                "or set the objective function to 0 with `lp_minimize(0)`",
                sep = "\n"
            )
        ))
    }

    objective <- as.L_objective.lp_problem(problem)
    constraints <- as.L_constraint.lp_problem(problem)

    types <- character(ncol(problem))
    lower <- numeric(ncol(problem))
    upper <- numeric(ncol(problem))

    for (x in problem$variables) {
        types[x$ind] <- x$type
        lower[x$ind] <- x$lower
        upper[x$ind] <- x$upper
    }

    # Bound indices and bounds
    li <- which(lower != 0)
    ui <- which(is.finite(upper))
    lb <- lower[li]
    ub <- upper[ui]

    bounds <- ROI::V_bound(
        li = li, ui = ui,
        lb = lb, ub = ub,
        nobj = ncol(problem)
    )

    ROI::OP(
        objective = objective,
        maximum = maximize,
        types = types,
        bounds = bounds,
        constraints = constraints
    )
}

#' @importFrom ROI as.OP
#' @export
ROI::as.OP

#' @export
as.OP.lp_problem <- function(x) make_model(x)

#' Solve a Model
#'
#' Find the optimum of an Optimization Model created with [make_model()] or [ROI::OP()].
#' Used internally in [lp_solve()].
#'
#' @param model An `OP` object created with [make_model()] or [ROI::OP()].
#' @inheritParams lp_solve
#'
#' @export
#' @example inst/examples/example_solve_steps.R
solve_model <- function(model, solver, ...) {
    if (!inherits(model, "OP")) {
        abort("Model must be an `OP` object created with `make_model()`.")
    }

    applicable <- ROI::ROI_applicable_solvers(model)

    if (length(applicable) == 0L) {
        rlang::abort(c(
            "No applicable solvers loaded.",
            ">" = "Use `library(ROI)` to load all installed solvers.",
            ">" = "Use `library(ROI.plugin.<solver>)` to load a specific solver.",
            "i" = glue::glue(
                "See https://roi.r-forge.r-project.org/installation.html#ROI_plug-ins",
                " for instructions on how to install each solver."
            )
        ))
    }

    dots <- rlang::dots_list(...)
    out <- ROI::ROI_solve(model, solver = solver, control = dots)
    out$model <- model
    return(out)
}

#' Prettify the Solution of a Model.
#'
#' Takes a problem and its solution and prettifies the solution. Used internally
#' in [lp_solve()].
#'
#' @param problem An [lp_problem()].
#' @param solution A list created with [solve_model()] or [ROI::ROI_solve()].
#' @inherit lp_solve
#' @export
#'
#' @example inst/examples/example_solve_steps.R
pretty_solution <- function(problem, solution, binary_as_logical = FALSE) {
    check_problem(problem, field_name = "problem")
    check_roi_solution(solution)

    if (length(solution$solution) == 0) {
        out <- list(
            objective = NA_real_,
            variables_vec = rep(NA_real_, ncol(problem)),
            status = solution$status,
            message = solution$message,
            model = solution$model
        ) |> structure(class = "lp_solution")

        return(out)
    }

    vars <- purrr::map(problem$variables, function(x) {
        out <- array(
            solution$solution[x$ind],
            dim = dim(x$ind),
            dimnames = dimnames(x$ind)
        )

        if (length(x) == 1L  &&  identical(names(dimnames(x)), "scalar")) {
            out <- unname(out[1])
        }

        if (x$binary && binary_as_logical) {
            out <- out > 0.5
        }

        out
    })

    als <- compute_aliases(problem, solution$solution)
    objective <- solution$objval + problem$objective$add

    list(
        objective = objective,
        variables = vars,
        aliases = als,
        variables_vec = solution$solution,
        status = solution$status,
        message = solution$message,
        model = solution$model
    ) |> structure(class = "lp_solution")
}

# Methods --------------------------------------

#' @export
print.lp_solution <- function(x, ...) {
    if (!is.na(x$objective)) {
        print(x["variables"])

        if (length(x$aliases) > 0L) {
            print(x["aliases"])
        }

        print(x["objective"])
    }

    if (x$status$code == 0) {
        cat("$status$code = 0  (Optimal)\n")
    } else {
        cat("$status$code =", x$status$code, "\n")
        cat("$status$msg\n")
        print(x$status$msg)
    }

    cat("\nFields:\n")
    cat(paste0("-- $", names(x), " --\n"), sep = "")
}
