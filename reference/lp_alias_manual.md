# Manually Define an Alias by Assigning its Values

This function does the same as
[`lp_alias()`](https://benet1one.github.io/lpsugar/reference/lp_alias.md),
but gives more freedom. It provides a better syntax for defining aliases
that cannot be defined in a single line of code.

## Usage

``` r
lp_alias_manual(.problem, definition, expression)

lp_impvar_manual(.problem, definition, expression)
```

## Arguments

- .problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)
  object.

- definition:

  Name and dimensions of the variable.

  - If the variable is a scalar, simply type it's name.

    - `lp_variable(x)`

  - If the variable is a vector, type it's name and indices. Indices can
    also be named.

    - `lp_variable( v[1:5] )`

    - `lp_variable( v[letters[1:5]] )`

    - `lp_variable( v[ind = letters[1:5]] )`

    - `ind <- letters[1:5]; lp_variable( v[ind] )`

    The last two have the same result.

  - If the variable is a matrix or n-dimensional array, type it's name
    and the indices for every dimension
    ([`base::dimnames()`](https://rdrr.io/r/base/dimnames.html)),
    comma-separated.

    - `lp_variable( mat[1:2, 1:3] )`

    - `lp_variable( arr[1:2, 1:3, 1:2] )`

- expression:

  Code to assign values to the alias. The values must be numeric or
  `<lp_variable>`. See examples.

## Value

The `.problem` with the added alias in `$aliases`.

## See also

[`lp_alias()`](https://benet1one.github.io/lpsugar/reference/lp_alias.md)

## Examples

``` r
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
```
