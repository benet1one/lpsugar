## Simple Example ---------------------------

## We have a 4x4 matrix X
## We want an alias A, of length 4, such that
## A[i] <- i * X[i, i+1]
## A[4] <- 4

p <- lp_problem() |> 
    lp_variable(X[1:4, 1:4]) |> 
    lp_alias_manual(
        A[1:4], {
            for (i in 1:3) A[i] <- i * X[i, i+1]
            A[4] <- 4
        }
    )


## Stock Problem ---------------------------

## Say a company has a 'stock[p, t]' of product 'p' at the end of day 't'.
## with an initial stock is 'stock_t0[p]' at day 0.
## The decision variable is the 'production[p, t]'
## to satisfy the 'sells[p, t]'

products <- c("Notebooks", "Planners")
days <- 1:3

stock_t0 <- c(
    Notebooks = 40, 
    Planners  = 15
)
sells <- c(
    20, 30, 10,
    10, 25, 40
) |> parameter(products, days)

stock_problem <- lp_problem() |> 
    lp_variable(
        production[products, days], 
        lower = 0, upper = 20
    ) |> 
    lp_alias_manual(
        stock[products, days], for (p in products) for (t in days) {
            if (t == 1) {
                stock[p, t] <- stock_t0[p]   + production[p, t] - sells[p, t]
            } else {
                stock[p, t] <- stock[p, t-1] + production[p, t] - sells[p, t]
            }
        }
    ) |> 
    lp_constraint(
        ## Non-negative stock
        stock >= 0,
        ## Capacity to store at most 50 products
        for (t in 1:3) {
            sum(stock[, t]) <= 50
        }
    )
