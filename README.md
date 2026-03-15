
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lpsugar

lpsugar is an AML (Algebraic Modeling Language) inspired in R’s
vectorized syntax. Define and Mixed Integer Linear Programs (MILP).

## Installation

You can install the development version of lpsugar from
[GitHub](https://github.com/benet1one/lpsugar) with:

``` r
# install.packages("pak")
pak::pak("benet1one/lpsugar")
```

## Quick Example

Start with a simple problem to show the basic syntax.

$$
\begin{array}{rl}
  \text{max} & x+y \\
  \text{st} & 2x + y \le 8 \\
            & 2x + 3y \le 12
\end{array}
$$

Let’s write the problem in lpsugar and solve it!

``` r
library(lpsugar)

my_problem <- lp_problem() |> 
    lp_variable(x) |> 
    lp_variable(y) |> 
    lp_maximize(x + y) |> 
    lp_constraint(
        2*x + y <= 8,
        2*x + 3*y <= 12
    )

my_solution <- lp_solve(my_problem)
my_solution$variables
#> $x
#> [1] 3
#> 
#> $y
#> [1] 2
my_solution$objective
#> [1] 5
```

## Transportation Problem

Here’s an example solving the classic transportation problem. We shall
transport `u[f, w]` units from factory `f` to warehousse `w`.

``` r
factory <- 1:3
warehouse <- c("A", "B", "C", "D")

supply <- c(20, 25, 15)
names(supply) <- factory

demand <- c(8, 10, 12, 10)
names(demand) <- warehouse

cost <- c(
    2, 7, 3, 1,
    6, 2, 9, 2,
    4, 6, 2, 8
)

cost <- matrix(cost, nrow = 3, byrow = TRUE)
dimnames(cost) <- list(factory = factory, warehouse = warehouse)

transportation_problem <- lp_problem() |> 
    lp_variable(u[factory, warehouse], integer = TRUE, lower = 0) |> 
    lp_minimize(sum(u * cost)) |> 
    lp_constraint(
        for (f in factory)   sum(u[f, ]) <= supply[f],
        for (w in warehouse) sum(u[, w]) >= demand[w]
    )

transportation_solution <- lp_solve(transportation_problem)
transportation_solution$variables
#> $u
#>        warehouse
#> factory A  B  C  D
#>       1 8  0  0 10
#>       2 0 10  0  0
#>       3 0  0 12  0
transportation_solution$objective
#> [1] 70
```
