# Regression and Classification

``` r

library(lpsugar)
library(ROI.plugin.highs)
```

This vignette focuses on different regression techniques that can be
solved using linear models.

\\ y_i = \sum\_{j=1}^k{\beta_j x\_{ij}} + e_i \\ We start by generating
the data we’ll use.

``` r

set.seed(123)
n <- 50
k <- 3

true_beta <- c(2, 3, -5)

x <- rpois(n*k, 6) |> matrix(n, k)
y <- x %*% true_beta + rnorm(n)
```

    #>  x1 x2 x3          y
    #>   5  2  6 -12.974429
    #>   8  5  5   5.715227
    #>   5  8  6   2.779282
    #>   9  3 10 -22.818697
    #>  10  6  6   7.861109
    #> ... and 45 more rows

## Least Absolute Deviation

The objective of Least Absolute Deviation (LAD) regression is to
minimize the absolute residuals \\e_i\\.

\\ \min{\sum\_{i=1}^n{e_i}} \\

Then the problem can be written like this:

\\ \begin{array}{rl} \min\_{\beta,e} & \sum\_{i=1}^n{e_i} \\ \text{st} &
e_i \ge \|y_i - \hat{y_i}\| \\ \text{where} & \hat{y_i} =
\sum\_{j=1}^k{\beta_j x\_{ij}} \end{array} \\

Believe it or not, the constraint is linear, despite containing an
absolute value. `lpsugar` will simply expand the constraint into two.
Together, they’re equivalent to the absolute value constraint.

\\ \begin{aligned} & +e_i \ge y_i - \hat{y_i} \\ & -e_i \le y_i -
\hat{y_i} \end{aligned} \\

Let’s write the problem in `lpsugar`.

``` r

n <- nrow(x)
k <- ncol(x)

lad <- lp_problem() |> 
    lp_var(beta[1:k]) |> 
    lp_var(e[1:n], lower = 0) |> 
    lp_min(sum(e)) |> 
    lp_alias(yhat = x %*% beta) |> 
    lp_con(e >= abs(y - yhat)) |> 
    lp_solve(solver = "highs")
```

The estimated \\\hat{\beta}\\ is quite similar to the `true_beta`
\\\beta\\.

``` r

cbind(
    true_beta = true_beta,
    lad_beta = lad$variables$beta |> round(2)
)
#>      true_beta lad_beta
#> [1,]         2     1.99
#> [2,]         3     2.96
#> [3,]        -5    -4.97
```

And the absolute deviation is:

``` r

lad$objective
#> [1] 32.79285
```

## Support Vector Machines

Support Vector Machines (SVM) are a binary classification method, that
uses a hyperplane to separate the groups.

\\ \begin{array}{rl} x\_{ij} \in \mathbb{R} &\quad i \in \[1,n\],\\ j\in
\[1,k\] \\ y_i \in \\\text{TRUE},\\ \text{FALSE}\\ &\quad i \in \[1,n\]
\end{array} \\

Usually, \\y\\ is represented as \\y_i \in \\-1, +1\\\\, but I believe
it’s easier to understand as a boolean variable.

### Basic Idea

An SVM finds a vector \\\\w_j\\\_{j\in\[1,k\]}\\ and scalar \\b\\ that
represents a hyperplane:

\\ \sum\_{j=1}^k{w_j X_j} + b = 0 \\

Classifications works like this. If the *signed distance* from
\\x\_{ij}\\ to the hyperplane is positive, \\y_i\\ is predicted `TRUE`.
If it’s negative, \\y_i\\ is preficted `FALSE`.

\\ d_i = \sum\_{j=1}^k{w\_{j} x\_{ij}} + b \\

\\ \begin{aligned} d_i \> 0 & \Rightarrow y_i=\text{TRUE} \\ d_i \< 0 &
\Rightarrow y_i=\text{FALSE} \end{aligned} \\

With this randomly generated data, we can find a line (which is an
\\\mathbb{R}^2\\ hyperplane) that perfectly separates orange from blue.

![](Regression_files/figure-html/unnamed-chunk-7-1.png)

However, this is not the *best* separator. There are two methods of
finding a better one, a Hard Margin SVM and a Soft Margin SVM.

### Hard Margin

The goal of an SVM is to find the greatest possible *margin*. The margin
is the smallest distance from a point to the hyperplane. Minimizing the
margin is equivalent to minimizing the quadratic sum of \\w\\
coefficients.

\\ \min_w \sum\_{j=1}^k{w_j^2} \\

And these constraints ensure the prediction is always correct.

\\ \begin{array}{rl} d_i \ge +1 &\quad \forall i : y_i = \text{TRUE} \\
d_i \le -1 &\quad \forall i : y_i = \text{FALSE} \end{array} \\

The Hard Margin SVM can be written in `lpsugar` with the following code.
The problem has a quadratic objective function and linear constraints.

``` r

svm_hard <- lp_problem() |> 
    lp_var(w[1:k]) |> 
    lp_var(b) |> 
    lp_min(sum(w^2)) |> 
    lp_alias_manual(
        signed_dist[1:n],
        for (i in 1:n) signed_dist[i] = sum_over(w[j] * x[i,j], j = 1:k) + b
    ) |> 
    lp_con(
        for (i in which(y))  signed_dist[i] >= +1,
        for (i in which(!y)) signed_dist[i] <= -1
    ) |> 
    lp_solve(solver = "highs")
```

This is the result. The continuous line represent the hyperplane, and
the dashed lines represent the points where the hyperplane formula is
\\+1\\ and \\-1\\.

![](Regression_files/figure-html/unnamed-chunk-9-1.png)

### Soft Margin

Soft Margin SVMs allow some prediction errors. This means they support
data that cannot be linearly separated. A Hard Margin SVM would fail to
find a feasible solution with this data.

![](Regression_files/figure-html/unnamed-chunk-10-1.png)

We will introduce a slack variable \\\xi_i \ge 0\\ indicating the
prediction error for observation \\i\\. Naturally, we want to minimize
the prediction error, so we will update the objective function:

\\ \min_w \sum\_{j=1}^k{w_j^2} + C\sum\_{i=1}^n{\xi_i} \\

Where \\C\\ is known as the Regularization Hyperparameter.

The constraints will now allow some flexibility by adding the slack
variables \\\xi\\:

\\ \begin{array}{rl} d_i \ge +1 - \xi_i &\quad \forall i : y_i =
\text{TRUE} \\ d_i \le -1 + \xi_i &\quad \forall i : y_i = \text{FALSE}
\end{array} \\

In `lpsugar`, the Soft Margin SVM can be coded as follows.

``` r

C <- 5 # Regularization Hyperparameter

svm_soft <- lp_problem() |> 
    lp_var(w[1:k]) |> 
    lp_var(b) |> 
    lp_var(slack[1:n], lower = 0) |> 
    lp_min(sum(w^2) + C * sum(slack)) |> 
    lp_alias_manual(
        signed_dist[1:n],
        for (i in 1:n) signed_dist[i] = sum_over(w[j] * x[i,j], j = 1:k) + b
    ) |> 
    lp_con(
        for (i in which(y))  signed_dist[i] >= +1 - slack[i],
        for (i in which(!y)) signed_dist[i] <= -1 + slack[i]
    ) |> 
    lp_solve(solver = "highs")
```

Notice that all blue values fall on top of the hyperplane, but some
orange values are incorrectly predicted.

![](Regression_files/figure-html/unnamed-chunk-12-1.png)
