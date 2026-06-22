
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/benet1one/lpsugar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/benet1one/lpsugar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Linear, Quadratic and Nonlinear Programming in R

`lpsugar` is an AML (Algebraic Modeling Language) inspired in R’s
vectorized syntax, used to define and solve Mixed Integer Linear
Problems (MILP), Mixed Integer Quadratic Problems (MIQP) and General
Nonlinear Problems (NLP).

This package is in development, and being made by a solo student. For a
safer choice, I recommend using
[ompr](https://dirkschumacher.github.io/ompr/).

## Installation

You can install the development version of `lpsugar` from
[GitHub](https://github.com/benet1one/lpsugar) with:

``` r
# install.packages("pak")
pak::pak("benet1one/lpsugar")
```

## Quick Examples

Start with a simple problem to show the basic syntax.

$$\begin{array}{rl}
  \text{max} & x+y \\
  \text{st} & 2x + y \le 8 \\
            & 2x + 3y \le 12
\end{array}$$

Let’s write the problem in `lpsugar` and solve it!

``` r
library(lpsugar)
library(ROI.plugin.highs) # Use any ROI solver you like!

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

### Transportation Problem

Here’s an example solving the classic transportation problem. We shall
transport `x[f, m]` units from factory `f` to market `m`, subject to a
`supply[f]` and a `demand[m]`. We will minimize the total transportation
`cost`.

``` r
supply <- c(BLB = 20, ACO = 25, GRN = 15)
demand <- c(Madrid = 8, Barcelona = 10, Valencia = 12, Seville = 10)

factory <- names(supply)
market <- names(demand)

cost <- c(
    2, 7, 3, 1,
    6, 2, 9, 2,
    4, 6, 2, 8
) |> parameter(factory, market)

cost
#>        market
#> factory Madrid Barcelona Valencia Seville
#>     BLB      2         7        3       1
#>     ACO      6         2        9       2
#>     GRN      4         6        2       8

transportation_problem <- lp_problem() |> 
    lp_variable(
        x[factory, market], 
        integer = TRUE, 
        lower = 0
    ) |> 
    lp_minimize(
        sum_over(f = factory, m = market, x[f, m] * cost[f, m])
    ) |> 
    lp_constraint(
        for (f in factory) sum(x[f, ]) <= supply[f],
        for (m in market)  sum(x[, m]) >= demand[m]
    )

transportation_solution <- lp_solve(transportation_problem)
transportation_solution$variables$x
#>        market
#> factory Madrid Barcelona Valencia Seville
#>     BLB      8         0        0      10
#>     ACO      0        10        0       0
#>     GRN      0         0       12       0
transportation_solution$objective
#> [1] 70
```

## Vignettes

For more examples, see the
[vignettes](https://benet1one.github.io/lpsugar/articles/) in the
package website.
