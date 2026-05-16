# regression

``` r

library(lpsugar)
library(ROI)
#> ROI: R Optimization Infrastructure
#> Registered solver plugins: nlminb, highs.
#> Default solver: auto.
```

This vignette focuses on different regression techniques that can be
solved using linear models.

``` math
y_i = \sum_{j=1}^k{\beta_j x_{ij}} + e_i
```
We start by generating the data we’ll use.

``` r

set.seed(123)
n <- 50
k <- 3

true_beta <- c(2, 3, -5)

x <- matrix(rpois(n*k, 6), nrow = n, ncol = k)
x[, 1] <- 1

e <- rnorm(n)
y <- (x %*% true_beta) + e

head(x)
#>      [,1] [,2] [,3]
#> [1,]    1    2    6
#> [2,]    1    5    5
#> [3,]    1    8    6
#> [4,]    1    3   10
#> [5,]    1    6    6
#> [6,]    1    4    9
head(y)
#>            [,1]
#> [1,] -20.974429
#> [2,]  -8.284773
#> [3,]  -5.220718
#> [4,] -38.818697
#> [5,] -10.138891
#> [6,] -30.994236
```

## Least Absolute Deviation

The objective of Least Absolute Deviation (LAD) regression is to
minimize the absolute residuals.

``` math
\min{\sum_{i=1}^n{|e_i|}}
```

The absolute value function $`|e|`$ is not linear, so we have to
separate each $`e_i`$ into:

``` math
e_i = e_i^+ - e_i^-
```
``` math
e_i^+ \ge 0,\ e_i^- \ge 0
```
Then the problem can be written like this:

``` math
\begin{array}{rl}
  \min & \sum_{i=1}^n{e_i^+ + e_i^-} \\
  \text{st} & e_i^+ \ge y_i - \hat{y_i} \\
            & e_i^- \ge \hat{y_i} - y_i \\
            & e_i^+ \ge 0 \\
            & e_i^- \ge 0 \\
  \text{where} & \hat{y_i} = \sum_{j=1}^k{\beta_j x_{ij}}
\end{array}
```

This works. The objective function attempts to push both $`e_i^+`$ and
$`e_i^-`$ down, but the constraints ensure that:

- If $`e_i > 0\ \Rightarrow\ e_i^+ = e_i,\ e_i^- = 0`$.

- If $`e_i < 0\ \Rightarrow\ e_i^+ = 0,\ e_i^- = |e_i|`$.

Let’s write the problem in `lpsugar`.

``` r

n <- nrow(x)
k <- ncol(x)

lad <- lp_problem() |> 
    lp_var(beta[1:k, 1]) |> 
    lp_var(e_pos[1:n], lower = 0) |> 
    lp_var(e_neg[1:n], lower = 0) |> 
    lp_min(sum(e_pos + e_neg)) |> 
    lp_alias(yhat = x %*% beta) |> 
    lp_con(
        pos = e_pos >= y - yhat,
        neg = e_neg >= yhat - y
    ) |> 
    lp_solve()

cbind(
    true_beta = true_beta,
    lad_beta = lad$variables$beta[, 1] |> round(2)
)
#>   true_beta lad_beta
#> 1         2     1.83
#> 2         3     2.98
#> 3        -5    -4.97
```
