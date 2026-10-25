# Show Control Arguments for a Solver

Get a data frame with the names of the solver's hyperparameters, for
using in
[`lp_solve()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md).

## Usage

``` r
solver_control_args(solver)
```

## Arguments

- solver:

  String, name of the solver.

## Value

A data frame with the names of the control arguments. Both options are
valid.

## Examples

``` r
solver_control_args("nlminb")
#>       control roi_control
#> 1       trace     verbose
#> 2    iter.max    max_iter
#> 3     rel.tol         tol
#> 4       start       start
#> 5     hessian           X
#> 6    eval.max           X
#> 7     abs.tol           X
#> 8       x.tol           X
#> 9      xf.tol           X
#> 10   step.min           X
#> 11   sing.tol           X
#> 12 scale.init           X
#> 13     diff.g           X
```
