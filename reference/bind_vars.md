# Concatenate variables and numbers.

Bind multiple variables and/or constants together.

## Usage

``` r
bind_vars(...)
```

## Arguments

- ...:

  Problem variables and/or numeric constants, vectors or arrays.

## Value

An `lp_variable`.

## Examples

``` r
# y[i] is a variable such that
# y[i] > y[i-1]
# Y0 is a parameter indicating what would be y[0]

Y0 <- 4

p <- lp_problem() |>
    lp_var(y[1:3]) |>
    lp_alias(y_full = bind_vars(Y0, y)) |>
    lp_con(for (i in 2:length(y_full))
        y_full[i] > y_full[i-1]
    )

p$constraints
#> 
#>  <unnamed> | n = 3 | for (i in 2:length(y_full)) y_full[i] > y_full[i - 1]
#> 
#>       y[1] y[2] y[3] dir  
#> [i=2] 1    0    0    >=  4
#> [i=3] -1   1    0    >=  0
#> [i=4] 0    -1   1    >=  0
#> 
```
