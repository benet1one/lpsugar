# Nonlinear

``` r

library(lpsugar)
library(ROI.plugin.nloptr)
```

## Constrained Maximum Likelihood

Say we have a list \\x\\ of vectors \\x_i = \\x\_{i1}, \dots,
x\_{im}\\\\. Each vector is generated from a \\\text{Normal}(\mu_i,
\sigma)\\. The catch is that each \\\mu\\ is greater than the previous:

\\ x\_{ij} \sim \text{Normal}(\mu_i, \sigma) \qquad \forall
i\in\[1,n\],\\ j\in\[1,m_i\] \\

\\ \mu_i \ge \mu\_{i-1} \qquad \forall i \in \[2,n\] \\

Let’s generate the data.

``` r

set.seed(101)

n <- 5
m <- runif(n, 4, 8) |> round()

true_mu <- 3 + cumsum(runif(n, 0, 1))
true_sigma <- 7

x <- list()

for (i in 1:n) {
    x[[i]] <- rnorm(m[i], mean = true_mu[i], sd = true_sigma) |> round(2)
}

print(x)
#> [[1]]
#> [1] 11.52  7.63  2.51  9.72  1.74
#> 
#> [[2]]
#> [1]  7.57 -1.68 13.88 -6.38
#> 
#> [[3]]
#> [1]   2.56   2.87  -1.73   4.63  -1.51 -10.13   3.07
#> 
#> [[4]]
#> [1]  9.80  2.96 -5.41 10.05 -5.03  8.11  4.01
#> 
#> [[5]]
#> [1]  8.66  8.87 11.65  7.34 12.44
```

We will find \\\mu\\ and \\\sigma\\ by maximum likelihood. Here’s the
function that returns the loglikelihood. It’s arguments *must* be the
variables of the problem.

``` r

loglikelihood_x <- function(mu, sigma) {
    LL <- 0
    
    for (i in 1:n) {
        LL_i <- dnorm(x[[i]], mean = mu[i], sd = sigma, log = TRUE)
        LL <- LL + sum(LL_i)
    }
    
    return(LL)
}
```

Now we solve the problem.

``` r

constrained_mle <- lp_problem() |> 
    lp_var(mu[1:n]) |> 
    lp_var(sigma, lower = 0) |> 
    lp_max_fun(loglikelihood_x) |> 
    lp_con(
        for (i in 2:n) mu[i] >= mu[i-1]
    ) |> 
    lp_solve(
        solver = "nloptr.cobyla",
        start = list(mu = rep(0, n), sigma = 1)
    )

constrained_mle
#> – $variables
#> $mu
#> [1] 2.923783 2.923783 2.923783 3.493470 9.734194
#> 
#> $sigma
#> [1] 5.612845
#> 
#> – $objective
#> [1] -88.04821
#> 
#> – $status
#> Optimal Solution Found ✔
```

``` r

unconstrained_mu <- sapply(x, mean)
constrained_mu <- constrained_mle$variables$mu
```

    #>                  mu[1] mu[2] mu[3] mu[4] mu[5]
    #> unconstrained_mu  6.62  3.35 -0.03  3.50  9.79
    #> constrained_mu    2.92  2.92  2.92  3.49  9.73
    #> true_mu           3.30  3.88  4.22  4.84  5.39

Constrained optimization gives a better result.

``` r

cor(unconstrained_mu, true_mu)
#> [1] 0.3233756
cor(constrained_mu, true_mu)
#> [1] 0.771652
```

We also get an estimator for \\\sigma\\.

``` r

constrained_sigma <- constrained_mle$variables$sigma
```

    #>                   sigma
    #> constrained_sigma  5.61
    #> true_sigma         7.00
