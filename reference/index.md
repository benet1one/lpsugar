# Package index

## Define and solve a problem

- [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)
  : Create a Linear Problem

- [`lp_variable()`](https://benet1one.github.io/lpsugar/reference/lp_variable.md)
  [`lp_var()`](https://benet1one.github.io/lpsugar/reference/lp_variable.md)
  :

  Define a variable for an
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)

- [`lp_minimize()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md)
  [`lp_maximize()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md)
  [`lp_min()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md)
  [`lp_max()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md)
  : Set an objective function

- [`lp_constraint()`](https://benet1one.github.io/lpsugar/reference/lp_constraint.md)
  [`lp_con()`](https://benet1one.github.io/lpsugar/reference/lp_constraint.md)
  [`lp_subject_to()`](https://benet1one.github.io/lpsugar/reference/lp_constraint.md)
  :

  Add constraints to an
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)

- [`lp_delete_constraint()`](https://benet1one.github.io/lpsugar/reference/lp_delete_constraint.md)
  : Delete constraints

- [`lp_alias()`](https://benet1one.github.io/lpsugar/reference/lp_alias.md)
  [`lp_implicit_variable()`](https://benet1one.github.io/lpsugar/reference/lp_alias.md)
  [`lp_impvar()`](https://benet1one.github.io/lpsugar/reference/lp_alias.md)
  : Define an Alias or Implicit Variable (IMPVAR)

- [`lp_minimize_function()`](https://benet1one.github.io/lpsugar/reference/lp_objective_function.md)
  [`lp_maximize_function()`](https://benet1one.github.io/lpsugar/reference/lp_objective_function.md)
  [`lp_min_fun()`](https://benet1one.github.io/lpsugar/reference/lp_objective_function.md)
  [`lp_max_fun()`](https://benet1one.github.io/lpsugar/reference/lp_objective_function.md)
  : Set a General Nonlinear Objective Function

- [`lp_solve()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md)
  [`lp_find_feasible()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md)
  : Solve a Linear Problem

## Utilities

- [`sum_over()`](https://benet1one.github.io/lpsugar/reference/sum_over.md)
  : Index Based Summation
- [`parameter()`](https://benet1one.github.io/lpsugar/reference/parameter.md)
  : Set Dimensions and Names for Parameters

## Bind variables or constraints

- [`bind_vars()`](https://benet1one.github.io/lpsugar/reference/bind_vars.md)
  : Concatenate variables and numbers.
- [`bind_cons()`](https://benet1one.github.io/lpsugar/reference/bind_cons.md)
  : Define Multiple Constraints at Once

## Analyse the solution

- [`solution_summary()`](https://benet1one.github.io/lpsugar/reference/solution_summary.md)
  [`constraint_summary()`](https://benet1one.github.io/lpsugar/reference/solution_summary.md)
  [`bound_summary()`](https://benet1one.github.io/lpsugar/reference/solution_summary.md)
  [`compute_objective()`](https://benet1one.github.io/lpsugar/reference/solution_summary.md)
  [`compute_aliases()`](https://benet1one.github.io/lpsugar/reference/solution_summary.md)
  : Compute a Summary of a Solution or Point

## Manually solve a problem

- [`as.OP(`*`<lp_problem>`*`)`](https://benet1one.github.io/lpsugar/reference/as.OP.lp_problem.md)
  :

  Create a [`ROI::OP()`](https://rdrr.io/pkg/ROI/man/OP.html) object.

- [`pretty_solution()`](https://benet1one.github.io/lpsugar/reference/pretty_solution.md)
  : Prettify the Solution of a Model.
