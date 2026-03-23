
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/benet1one/lpsugar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/benet1one/lpsugar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# lpsugar

lpsugar is an AML (Algebraic Modeling Language) inspired in R’s
vectorized syntax, used to define and solve Mixed Integer Linear
Programs (MILP).

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
library(ROI) |> suppressMessages() # Needs to be loaded

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

## Production Problem

Given a set of limited `resources`, we can make `products`. Each product
takes a certain amount of resources and sells at a `price`. We wish to
maximize earnings.

``` r
available <- c(Material = 20, Labour = 10)
price <- c(A = 150, B = 220)

resources <- names(available)
products <- names(price)

requires <- c(
    # A, B
      6, 3, # Material
      2, 4  # Labour
      
) |> parameter(resources, products)

# parameter() sets the dimensions and names
requires
#>           products
#> resources  A B
#>   Material 6 3
#>   Labour   2 4

production_problem <- lp_problem() |> 
    lp_var(units[products], integer = TRUE, lower = 0) |> 
    lp_max(sum(price * units)) |> 
    lp_con(
        for (r in resources) {
            req_r <- requires[r, ]
            used_r <- sum(req_r * units)
            (used_r <= available[r])
        }
    )

production_solution <- lp_solve(production_problem)
production_solution$objective
#> [1] 590
production_solution$variables
#> $units
#> products
#> A B 
#> 1 2
```

## Transportation Problem

Here’s an example solving the classic transportation problem. We shall
transport `u[f, w]` units from factory `f` to warehousse `w`, subject to
a `supply[f]` and a `demand[w]`. We will minimize the total `cost`.

``` r
factory <- paste0("fct", 1:3)
warehouse <- c("A", "B", "C", "D")

supply <- c(20, 25, 15) |> parameter(factory)
demand <- c(8, 10, 12, 10) |> parameter(warehouse)

cost <- c(
    # A, B, C, D
      2, 7, 3, 1,
      6, 2, 9, 2,
      4, 6, 2, 8
      
) |> parameter(factory, warehouse)

cost
#>        warehouse
#> factory A B C D
#>    fct1 2 7 3 1
#>    fct2 6 2 9 2
#>    fct3 4 6 2 8

transportation_problem <- lp_problem() |> 
    lp_variable(u[factory, warehouse], integer = TRUE, lower = 0) |> 
    lp_minimize(sum(u * cost)) |> 
    lp_constraint(
        for (f in factory)   sum(u[f, ]) <= supply[f],
        for (w in warehouse) sum(u[, w]) >= demand[w]
        # Alternatively, using vectorization:
        # rowSums(u) <= supply,
        # colSums(u) >= demand
    )

transportation_solution <- lp_solve(transportation_problem)
transportation_solution$objective
#> [1] 70
transportation_solution$variables
#> $u
#>        warehouse
#> factory A  B  C  D
#>    fct1 8  0  0 10
#>    fct2 0 10  0  0
#>    fct3 0  0 12  0
```
