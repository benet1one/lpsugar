# Define an Alias or Implicit Variable (IMPVAR)

Create a 'fake' variable that can be used in constraints and objective
without adding complexity to the problem.

## Usage

``` r
lp_alias(.problem, ...)

lp_implicit_variable(.problem, ...)

lp_impvar(.problem, ...)
```

## Arguments

- .problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

- ...:

  Name-value pairs. The name will be the name of the alias. The value
  must be a linear function of previously defined variables or aliases.

## Value

The `.problem` with the added `$aliases`.

## Examples

``` r
# Aliases are meant to avoid having to compute the same values
# for multiple different constraints and possibly the objective function.

# Simple Example ----------------------------------

# Say we had x, y, and wanted to define z such that
# z[i] = 2*x[i] + y[n-i+1]

# One option is to use constraints
n <- 3
set <- letters[1:n]
obj_coef <- c(2, 1, 5)

p0 <- lp_problem() |>
    lp_variable(x[set], lower = 1) |>
    lp_variable(y[set], lower = 1) |>
    lp_variable(z[set]) |>
    lp_minimize(sum(z * obj_coef)) |>
    lp_constraint(
        some_constraint = (x + y >= 4),
        constraint_with_z = (z[1] <= 7),
        xyz = for (i in 1:n)  z[i] == 2*x[i] + y[n-i+1]
    )

# But this problem has 3 extra variables and 3 extra constraints:
p0$constraints
#> 
#>  some_constraint | n = 3 | (x + y >= 4)
#> 
#>                 x[a] x[b] x[c] y[a] y[b] y[c] z[a] z[b] z[c] dir  
#> some_constraint 1    0    0    1    0    0    0    0    0    >=  4
#> some_constraint 0    1    0    0    1    0    0    0    0    >=  4
#> some_constraint 0    0    1    0    0    1    0    0    0    >=  4
#> 
#> 
#>  constraint_with_z | n = 1 | (z[1] <= 7)
#> 
#>                   x[a] x[b] x[c] y[a] y[b] y[c] z[a] z[b] z[c] dir  
#> constraint_with_z 0    0    0    0    0    0    1    0    0    <=  7
#> 
#> 
#>  xyz | n = 3 | for (i in 1:n) z[i] == 2 * x[i] + y[n - i + 1]
#> 
#>          x[a] x[b] x[c] y[a] y[b] y[c] z[a] z[b] z[c] dir  
#> xyz[i=1] -2   0    0    0    0    -1   1    0    0    ==  0
#> xyz[i=2] 0    -2   0    0    -1   0    0    1    0    ==  0
#> xyz[i=3] 0    0    -2   -1   0    0    0    0    1    ==  0
#> 

# Instead, we can define 'z' as an alias
p1 <- lp_problem() |>
    lp_variable(x[set], lower = 1) |>
    lp_variable(y[set], lower = 1) |>
    lp_alias(
        z = 2*x + rev(y)
    ) |>
    lp_minimize(sum(z * obj_coef)) |>
    lp_constraint(
        some_constraint = x + y >= 4,
        constraint_with_z = (z[1] <= 7)
    )

# This problem no extra variables or constraints!
p1$constraints
#> 
#>  some_constraint | n = 3 | x + y >= 4
#> 
#>                 x[a] x[b] x[c] y[a] y[b] y[c] dir  
#> some_constraint 1    0    0    1    0    0    >=  4
#> some_constraint 0    1    0    0    1    0    >=  4
#> some_constraint 0    0    1    0    0    1    >=  4
#> 
#> 
#>  constraint_with_z | n = 1 | (z[1] <= 7)
#> 
#>                   x[a] x[b] x[c] y[a] y[b] y[c] dir  
#> constraint_with_z 2    0    0    0    0    1    <=  7
#> 

# The solution is presented differently
library(ROI) |> suppressMessages()
lp_solve(p0) [c("variables", "aliases")] # z is a variable
#> $variables
#> $variables$x
#> set
#> a b c 
#> 2 1 1 
#> 
#> $variables$y
#> set
#> a b c 
#> 2 3 3 
#> 
#> $variables$z
#> set
#> a b c 
#> 7 5 4 
#> 
#> 
#> $aliases
#> list()
#> 
lp_solve(p1) [c("variables", "aliases")] # z is an alias
#> $variables
#> $variables$x
#> set
#> a b c 
#> 2 1 1 
#> 
#> $variables$y
#> set
#> a b c 
#> 2 3 3 
#> 
#> 
#> $aliases
#> $aliases$z
#> set
#> a b c 
#> 7 5 4 
#> 
#> 



# More Interesting Example ------------------------
Rows <- letters[1:3]
Cols <- LETTERS[1:2]

min_col_cumsum <- c(
    1, 0,
    1, 0,
    5, 3
) |> parameter(Rows, Cols)

max_col_cumsum <- c(
    4, 3,
    5, 9,
    9, 7
) |> parameter(Rows, Cols)


# Without Aliases
p0 <- lp_problem() |>
    lp_var(x[Rows, Cols]) |>
    lp_max(sum(x)) |>
    lp_con(
        min = for (j in Cols) for (i in seq_along(Rows)) {
            sum(x[1:i, j]) >= min_col_cumsum[i, j]
        },
        max = for (j in Cols) for (i in seq_along(Rows)) {
            sum(x[1:i, j]) <= max_col_cumsum[i, j]
        },
        A_less_than_B = for (i in seq_along(Rows)) {
            sum(x[1:i, "A"]) <= sum(x[1:i, "B"])
        }
    )

# With an Alias
p1 <- lp_problem() |>
    lp_var(x[Rows, Cols]) |>
    lp_alias(col_cumsum = apply(x, 2, cumsum)) |>
    lp_max(sum(x)) |>
    lp_con(
        min = (col_cumsum >= min_col_cumsum),
        max = (col_cumsum <= max_col_cumsum),
        A_less_than_B = (col_cumsum[, "A"] <= col_cumsum[, "B"])
    )

# The problems are equivalent
p0$constraints
#> 
#>  min | n = 6 | for (j in Cols) for (i in seq_along(Rows)) { ... }
#> 
#>                 x[a,A] x[b,A] x[c,A] x[a,B] x[b,B] x[c,B] dir  
#> min[i=1, j="A"] 1      0      0      0      0      0      >=  1
#> min[i=2, j="A"] 1      1      0      0      0      0      >=  1
#> min[i=3, j="A"] 1      1      1      0      0      0      >=  5
#> min[i=1, j="B"] 0      0      0      1      0      0      >=  0
#> min[i=2, j="B"] 0      0      0      1      1      0      >=  0
#> min[i=3, j="B"] 0      0      0      1      1      1      >=  3
#> 
#> 
#>  max | n = 6 | for (j in Cols) for (i in seq_along(Rows)) { ... }
#> 
#>                 x[a,A] x[b,A] x[c,A] x[a,B] x[b,B] x[c,B] dir  
#> max[i=1, j="A"] 1      0      0      0      0      0      <=  4
#> max[i=2, j="A"] 1      1      0      0      0      0      <=  5
#> max[i=3, j="A"] 1      1      1      0      0      0      <=  9
#> max[i=1, j="B"] 0      0      0      1      0      0      <=  3
#> max[i=2, j="B"] 0      0      0      1      1      0      <=  9
#> max[i=3, j="B"] 0      0      0      1      1      1      <=  7
#> 
#> 
#>  A_less_than_B | n = 3 | for (i in seq_along(Rows)) { ... }
#> 
#>                    x[a,A] x[b,A] x[c,A] x[a,B] x[b,B] x[c,B] dir  
#> A_less_than_B[i=1] 1      0      0      -1     0      0      <=  0
#> A_less_than_B[i=2] 1      1      0      -1     -1     0      <=  0
#> A_less_than_B[i=3] 1      1      1      -1     -1     -1     <=  0
#> 
p1$constraints
#> 
#>  min | n = 6 | (col_cumsum >= min_col_cumsum)
#> 
#>     x[a,A] x[b,A] x[c,A] x[a,B] x[b,B] x[c,B] dir  
#> min 1      0      0      0      0      0      >=  1
#> min 1      1      0      0      0      0      >=  1
#> min 1      1      1      0      0      0      >=  5
#> min 0      0      0      1      0      0      >=  0
#> min 0      0      0      1      1      0      >=  0
#> min 0      0      0      1      1      1      >=  3
#> 
#> 
#>  max | n = 6 | (col_cumsum <= max_col_cumsum)
#> 
#>     x[a,A] x[b,A] x[c,A] x[a,B] x[b,B] x[c,B] dir  
#> max 1      0      0      0      0      0      <=  4
#> max 1      1      0      0      0      0      <=  5
#> max 1      1      1      0      0      0      <=  9
#> max 0      0      0      1      0      0      <=  3
#> max 0      0      0      1      1      0      <=  9
#> max 0      0      0      1      1      1      <=  7
#> 
#> 
#>  A_less_than_B | n = 3 | (col_cumsum[, "A"] <= col_cumsum[, "B"])
#> 
#>               x[a,A] x[b,A] x[c,A] x[a,B] x[b,B] x[c,B] dir  
#> A_less_than_B 1      0      0      -1     0      0      <=  0
#> A_less_than_B 1      1      0      -1     -1     0      <=  0
#> A_less_than_B 1      1      1      -1     -1     -1     <=  0
#> 

# Notice it's not a variable, thus not adding complexity to the problem
p1$variables
#> $x
#> Real variable 'x[Rows, Cols]'
#> 
# Instead you can find it in $aliases
p1$aliases
#> $col_cumsum
#> $coef
#>      x[a,A] x[b,A] x[c,A] x[a,B] x[b,B] x[c,B]
#> [1,]      1      0      0      0      0      0
#> [2,]      1      1      0      0      0      0
#> [3,]      1      1      1      0      0      0
#> [4,]      0      0      0      1      0      0
#> [5,]      0      0      0      1      1      0
#> [6,]      0      0      0      1      1      1
#> with class 'robust_index' from package 'lpsugar'
#> 
#> $add
#>      [,1]
#> [1,]    0
#> [2,]    0
#> [3,]    0
#> [4,]    0
#> [5,]    0
#> [6,]    0
#> with class 'robust_index' from package 'lpsugar'
#> 
#> 

# Aliases are computed from the final solution
library(ROI) |> suppressMessages()
s <- lp_solve(p1)
s$variables
#> $x
#>     Cols
#> Rows A  B
#>    a 1  3
#>    b 0  6
#>    c 6 -2
#> 
s$aliases
#> $col_cumsum
#>   A B
#> a 1 3
#> b 1 9
#> c 7 7
#> 
```
