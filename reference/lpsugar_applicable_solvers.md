# List Applicable and Available Solvers

Show which solvers can solve a problem. Applicable solvers must be
installed and loaded, whereas Available solvers needn't be installed.

## Usage

``` r
lpsugar_applicable_solvers(problem)

lpsugar_available_solvers(problem)
```

## Arguments

- problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)
  or a [`ROI::OP()`](https://rdrr.io/pkg/ROI/man/OP.html).

## Value

- `lpsugar_applicable_solvers` returns a character vector with the
  solver names.

- `lpsugar_available_solvers` returns a `data.frame` with information on
  the solvers.

## Details

- `lpsugar_applicable_solvers` returns a character vector of solver
  names, which can be used in `lp_solve(solver = _)`. It lists solvers
  which:

  - Can solve the `problem`.

  - Are installed and have been loaded with
    [`library(ROI)`](https://roi.r-forge.r-project.org/) or
    `library(ROI.plugin.<solver>)`.

- `lpsugar_available_solvers` returns a `data.frame` with information on
  the solvers. It lists solvers which:

  - Can solve the `problem`.

  - Do not need to be installed.

Note since Nonlinear Solvers are also applicable to Linear and Quadratic
problems, they will also be listed.

## See also

[`ROI::ROI_applicable_solvers()`](https://rdrr.io/pkg/ROI/man/ROI_applicable_solvers.html),
[`ROI::ROI_available_solvers()`](https://rdrr.io/pkg/ROI/man/ROI_available_solvers.html).

## Examples

``` r
library(ROI)
#> ROI: R Optimization Infrastructure
#> Registered solver plugins: nlminb, highs, nloptr.bobyqa, nloptr.crs2lm, nloptr.direct, nloptr.directL, nloptr.lbfgs, nloptr.neldermead, nloptr.newuoa, nloptr.sbplx, nloptr.stogo, nloptr.tnewton, nloptr.varmetric, nloptr.cobyla, nloptr.mma, nloptr.auglag, nloptr.isres, nloptr.slsqp, alabama.
#> Default solver: auto.

quadratic_prob <- lp_problem() |> 
    lp_var(x, lower = 0) |> 
    lp_var(y, lower = 0) |> 
    lp_min(x^2 + y) |> 
    lp_con(x + y >= 10)

# Installed and loaded quadratic solvers
lpsugar_applicable_solvers(quadratic_prob)
#> [1] "highs"         "nloptr.cobyla" "nloptr.mma"    "nloptr.auglag"
#> [5] "nloptr.isres"  "nloptr.slsqp"  "alabama"      
# All quadratic solvers
lpsugar_available_solvers(quadratic_prob) [c("Package", "Version", "Repository")]
#>                Package Version                       Repository
#> 1   ROI.plugin.alabama   1.0-0       https://CRAN.R-project.org
#> 2     ROI.plugin.cplex   0.3-0       https://CRAN.R-project.org
#> 6     ROI.plugin.highs   1.0-2       https://CRAN.R-project.org
#> 7      ROI.plugin.ipop   1.0-0       https://CRAN.R-project.org
#> 10     ROI.plugin.neos   1.0-0       https://CRAN.R-project.org
#> 11   ROI.plugin.nloptr   1.0-0       https://CRAN.R-project.org
#> 13     ROI.plugin.osqp   1.0-0       https://CRAN.R-project.org
#> 14  ROI.plugin.qpoases   1.0-2       https://CRAN.R-project.org
#> 15 ROI.plugin.quadprog   1.0-0       https://CRAN.R-project.org
#> 18  ROI.plugin.alabama   1.0-0 https://gitlab.com/roigrp/solver
#> 22   ROI.plugin.gurobi   1.0-0 https://gitlab.com/roigrp/solver
#> 23    ROI.plugin.highs   1.0-2 https://gitlab.com/roigrp/solver
#> 24    ROI.plugin.mosek   1.0-0 https://gitlab.com/roigrp/solver
#> 26     ROI.plugin.neos   1.0-0 https://gitlab.com/roigrp/solver
#> 27   ROI.plugin.nloptr   1.0-0 https://gitlab.com/roigrp/solver
#> 29     ROI.plugin.osqp   1.0-0 https://gitlab.com/roigrp/solver
#> 30  ROI.plugin.qpoases   1.0-2 https://gitlab.com/roigrp/solver
#> 31 ROI.plugin.quadprog   1.0-0 https://gitlab.com/roigrp/solver
```
