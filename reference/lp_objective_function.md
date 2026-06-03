# Set a General Nonlinear Objective Function

Optimize an R function.

## Usage

``` r
lp_minimize_function(.problem, fun, gradient = NULL, hessian = NULL)

lp_maximize_function(.problem, fun, gradient = NULL, hessian = NULL)

lp_min_fun(.problem, fun, gradient = NULL, hessian = NULL)

lp_max_fun(.problem, fun, gradient = NULL, hessian = NULL)
```

## Arguments

- .problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

- fun:

  Function to optimize. The function's arguments must match all defined
  [variables](https://benet1one.github.io/lpsugar/reference/lp_variable.md).

- gradient:

  Function that returns the gradient vector.

- hessian:

  Function that returns the hessian matrix.

## Value

An
[`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)
with the new `$objective` function.

## See also

[`lp_minimize()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md)
to optimize linear or quadratic functions.
