# lpsugar

This vignette will teach you the basic elements of a linear problem and
how to define them with `lpsugar`.

``` r

library(lpsugar)
```

## Quick Example: Transportation Problem

The elements of a transportation problem are these:

- Sets
  - $`F`$ - `factory`
  - $`M`$ - `market`
- Parameters
  - $`s`$ - `supply` produced by each `factory`.
  - $`d`$ - `demand` on each `market`.
  - $`c`$ - `cost` to transport a unit from each `factory` to each
    `market`.
- Variable
  - $`t`$ - `transport` products from each `factory` to each `market`.
- Objective Function
  - $`\min \sum_{f \in F} \sum_{m \in M} (c_{fm} t_{fm})`$ -
    `minimize sum(cost * transport)`
- Constraints
  - $`\sum_{m\in M}({t_{fm}}) \le s_f \quad \forall f\in F`$ -
    `for (f in factory) sum(transport[f, ]) <= supply[f]`
  - $`\sum_{f\in F}({t_{fm}}) \ge d_m \quad \forall m\in M`$ -
    `for (m in market) sum(transport[, m]) >= demand[m]`

Let’s define the problem in `lpsugar`.

``` r

# Sets
factory <- c("A", "B")
market  <- c("Barcelona", "Paris", "Rome")

# Parameters
supply <- c(A = 20, B = 50)
demand <- c(Barcelona = 30, Paris = 15, Rome = 25)
cost <- c(
    # Bar, Par, Rome
       30,  12,  32,
       16,  18,  40
) |> parameter(factory, market)

# Problem
transportation_problem <- lp_problem() |> 
    lp_variable(
        transport[factory, market],
        lower = 0
    ) |> 
    lp_minimize(
        sum(cost * transport)
    ) |> 
    lp_constraint(
        for (f in factory) sum(transport[f, ]) <= supply[f],
        for (m in market)  sum(transport[, m]) >= demand[m]
    )
```

To solve it, you can use any of `ROI`’s applicable solvers. One option
is to load only the desired solver with `library(ROI.plugin.<solver>)`
and another is to load all of `ROI` with
[`library(ROI)`](https://roi.r-forge.r-project.org/) and let it choose
the solver automatically.

``` r

library(ROI)
#> ROI: R Optimization Infrastructure
#> Registered solver plugins: nlminb, highs.
#> Default solver: auto.
solution <- lp_solve(transportation_problem)
solution$objective
#> [1] 1590
solution$variables
#> $transport
#>        market
#> factory Barcelona Paris Rome
#>       A         0     0   20
#>       B        30    15    5
```

## Basic Elements

### Sets

Sets are vectors of indices that a variable or parameter will use. For
instance, we could define a set $`S=\{a,b,c\}`$ to use for a parameter
$`\{w_i\}_{i \in S}`$ and a variable $`\{x_i\}_{i \in S}`$.

In `lpsugar` we define sets and parameters as base R objects. Variables
are defined inside a problem.

``` r

S <- c("a", "b", "c")
w <- c(a = 3, b = 2, c = 6)

p <- lp_problem() |>
    lp_variable(x[S])

w
#> a b c 
#> 3 2 6
p$variables$x
#> Real variable 'x[S]'
```

Note: Sets can be numeric, but they must go from `1:n`, where `n` is the
length of the set. This ensures indexing works as expected (`x[1]` is
always equal to `x["1"]`).

### Parameters

Parameters are simply fixed values with named dimensions. It’s not
necessary to name their dimensions, but it’s a good practice, and it
pays off in the long run.

For vectors, I recommend using R’s name-value syntax, but it’s also
possible to use `parameter`. These are all different alternatives for
defining $`\{w_i\}_{i \in S}`$:

``` r

S <- c("a", "b", "c")
w <- c(a = 3, b = 2, c = 6)
w <- c(3, 2, 6) |> stats::setNames(S)
w <- c(3, 2, 6) |> parameter(S)
```

For matrices, you can use `parameter` to define them comfortable within
the code. Let’s define $`\{m_{ij}\}_{i\in S,\ j\in R}`$.

``` r

S <- c("a", "b", "c")
R <- c("A", "B")

m <- c(
    1, 2,
    3, 4,
    5, 6
) |> parameter(S, R)

m
#>    R
#> S   A B
#>   a 1 2
#>   b 3 4
#>   c 5 6
```

Notice this works defining matrices by row (as in
`matrix(data, byrow = TRUE)`). If an object is already a matrix, I
recommend setting it’s names using `rownames` and `colnames`.

``` r

m <- matrix(1:6, nrow = 3, byrow = TRUE)
rownames(m) <- S
colnames(m) <- R

m
#>   A B
#> a 1 2
#> b 3 4
#> c 5 6
```

For n-dimensional arrays, `parameter` does not work (by design), so the
only option is to use `dimnames`. We will define a parameter
$`\{a_{ijk}\}_{i\in S,\ j\in R,\ k\in Q}`$

``` r

S <- c("a", "b", "c")
R <- c("A", "B")
Q <- c("Q1", "Q2")

a <- array(1:12, dim = c(3, 2, 2))
dimnames(a) <- list(S=S, R=R, Q=Q)

a
#> , , Q = Q1
#> 
#>    R
#> S   A B
#>   a 1 4
#>   b 2 5
#>   c 3 6
#> 
#> , , Q = Q2
#> 
#>    R
#> S   A  B
#>   a 7 10
#>   b 8 11
#>   c 9 12
```

### Variables

Variables are numbers that the solver will change in order to find the
optimal solution. In `lpsugar`, variables can be scalars, vectors, or
n-dimensional arrays. Their dimensions are defined using sets. For
instance, a variable $`\{x_{ij}\}_{i \in A,\ j \in B}`$ can be defined
as `lp_variable(x[A, B])`.

By default, their `lower` and `upper` bounds are `-Inf` to `+Inf`. It’s
possible to set a variable to `integer` or `binary`. Let us define a
vector variable $`\{x_i \in \mathbb{N}\}_{i \in 1..5}`$.

``` r

p <- lp_problem() |> 
    lp_variable(x[1:5], lower = 0, integer = TRUE)
```

### Objective Function

The objective function is a value that the solver will try to minimize
or maximize. It is set using
[`lp_minimize()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md)
or
[`lp_maximize()`](https://benet1one.github.io/lpsugar/reference/lp_objective.md).

Let us minimize the following sum:

``` math
\sum_{i \in I}\sum_{j \in J}{s_j x_{ij}}
```

We’ll use
[`sum_over()`](https://benet1one.github.io/lpsugar/reference/sum_over.md)
to sum over the indices.

``` r

I <- c(1, 2, 3)
J <- c("a", "b")

s <- c(a = 2, b = 5)

p <- lp_problem() |> 
    lp_variable(x[I, J]) |> 
    lp_minimize(
        sum_over(i=I, j=J, s[j] * x[i, j])
    )

p$objective$coef
#> x[1,a] x[2,a] x[3,a] x[1,b] x[2,b] x[3,b] 
#>      2      2      2      5      5      5
```

Sometimes the goal is to find any feasible solution. This can be done by
using
[`lp_find_feasible()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md)
instead of
[`lp_solve()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md).
Alternatively, the objective coeficients can be set to 0 by using
`lp_minimize(0)`. Then,
[`lp_solve()`](https://benet1one.github.io/lpsugar/reference/lp_solve.md)
also returns any feasible solution, with the objective value as 0.

``` r

pf <- p |> lp_minimize(0)
pf$objective
#> minimize  
#> $coef
#> x[1,a] x[2,a] x[3,a] x[1,b] x[2,b] x[3,b] 
#>      0      0      0      0      0      0
```

### Constraints

Constraints are equalities or inequalities that restrict the variables.
For instance, $`x \ge y+2`$ would be defined with the following code:

``` r

p <- lp_problem() |> 
    lp_variable(x) |> 
    lp_variable(y) |> 
    lp_constraint(x >= y + 2)
```

Often, constraints are defined in groups. In algebra, we use *forall*
$`(\forall)`$. Take this constraint:

``` math
k \cdot x_k \le y_{n-k+1} \qquad \forall k \in 1..n
```

It can be defined by replacing the *forall* with a `for` loop.

``` r

n <- 3

p <- lp_problem() |> 
    lp_variable(x[1:n]) |> 
    lp_variable(y[1:n]) |> 
    lp_constraint(
        my_constraint = for (k in 1:n) {
            k * x[k] <= y[n-k+1]
        }
    )

p$constraints
#> 
#>  my_constraint | n = 3 | for (k in 1:n) { ... }
#> 
#>                    x[1] x[2] x[3] y[1] y[2] y[3] dir  
#> my_constraint[k=1] 1    0    0    0    0    -1   <=  0
#> my_constraint[k=2] 0    2    0    0    -1   0    <=  0
#> my_constraint[k=3] 0    0    3    -1   0    0    <=  0
```

Since `lpsugar` supports vectorized operations, it’s possible to define
the same constraint as:

``` r

p2 <- p |> 
    lp_constraint(
        alt_constraint = {
            (1:n) * x <= rev(y)
        }
    )

p2$constraints["alt_constraint"]
#> 
#>  alt_constraint | n = 3 | { ... }
#> 
#>                x[1] x[2] x[3] y[1] y[2] y[3] dir  
#> alt_constraint 1    0    0    0    0    -1   <=  0
#> alt_constraint 0    2    0    0    -1   0    <=  0
#> alt_constraint 0    0    3    -1   0    0    <=  0
```

Constraints can contain multiple lines of code, which makes it possible
to divide complex operations more clearly. They can also contain `if`
statements, as long as the condition does not contain any variables.

``` r

n <- 4
y0 <- 5

p <- lp_problem() |> 
    lp_var(y[1:n], lower = 0) |> 
    lp_con(
        order_con = for (t in 1:n) {
            if (t == 1) {
                y_prior <- y0
            } else {
                y_prior <- y[t-1]
            }
            
            y[t] >= 2 * (y_prior + 1)
        }
    )

p$constraints
#> 
#>  order_con | n = 4 | for (t in 1:n) { ... }
#> 
#>                y[1] y[2] y[3] y[4] dir   
#> order_con[t=1] 1    0    0    0    >=  12
#> order_con[t=2] -2   1    0    0    >=  2 
#> order_con[t=3] 0    -2   1    0    >=  2 
#> order_con[t=4] 0    0    -2   1    >=  2
```
