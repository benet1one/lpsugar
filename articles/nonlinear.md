# Nonlinear

``` r

library(lpsugar)
library(ROI.plugin.nloptr)
```

## Constrained Maximum Likelihood

Say we have a list \\x\\ of vectors \\x_i = \\x\_{i1}, \dots,
x\_{im}\\\\. Each vector is generated from a \\\text{Normal}(\mu_i,
\sigma)\\. The catch is this: each \\\mu\\ is greater than the previous:

\\ \mu_i \ge \mu\_{i-1} \qquad \forall i \in \[2,n\] \\

Let’s generate the data.

``` r

set.seed(101)

n <- 5
m <- runif(n, 4, 8) |> round()

true_mu <- -0.5 + cumsum(runif(n, 0, 1))
true_sigma <- 7

x <- list()

for (i in 1:n) {
    x[[i]] <- rnorm(m[i], mean = true_mu[i], sd = true_sigma) |> round(2)
}

print(x)
#> [[1]]
#> [1]  8.02  4.13 -0.99  6.22 -1.76
#> 
#> [[2]]
#> [1]  4.07 -5.18 10.38 -9.88
#> 
#> [[3]]
#> [1]  -0.94  -0.63  -5.23   1.13  -5.01 -13.63  -0.43
#> 
#> [[4]]
#> [1]  6.30 -0.54 -8.91  6.55 -8.53  4.61  0.51
#> 
#> [[5]]
#> [1] 5.16 5.37 8.15 3.84 8.94
```

We will find \\\mu\\ and \\\sigma\\ by maximum likelihood. Here’s the
function that returns the loglikelihood. It’s arguments *must* be the
variables of the problem.

``` r

loglikelihood <- function(mu, sigma) {
    ll <- 0
    
    for (i in 1:n) {
        lli <- dnorm(x[[i]], mean = mu[i], sd = sigma, log = TRUE)
        ll <- ll + sum(lli)
    }
    
    return(ll)
}
```

Now we solve the problem.

``` r

constrained_mle <- lp_problem() |> 
    lp_var(mu[1:n]) |> 
    lp_var(sigma, lower = 0) |> 
    lp_max_fun(loglikelihood) |> 
    lp_con(for (i in 2:n) mu[i] >= mu[i-1]) |> 
    lp_solve(
        solver = "nloptr.cobyla",
        start = list(mu = rep(0, n), sigma = 1)
    )

constrained_mle
#> – $variables
#> $mu
#> [1] -0.5964356 -0.5964356 -0.5964356 -0.0398022  6.2840996
#> 
#> $sigma
#> [1] 5.612405
#> 
#> – $objective
#> [1] -88.04789
#> 
#> – $status
#> Optimal Solution Found ✔
```

    #>      unconstrained_means constrained_mle true_mu
    #> [1,]                3.12           -0.60   -0.20
    #> [2,]               -0.15           -0.60    0.38
    #> [3,]               -3.53           -0.60    0.72
    #> [4,]                0.00           -0.04    1.34
    #> [5,]                6.29            6.28    1.89

Constrained optimization gives a better result.

``` r

cor(unconstrained_means, true_mu)
#> [1] 0.3233756
cor(constrained_mle$variables$mu, true_mu)
#> [1] 0.7702947
```
