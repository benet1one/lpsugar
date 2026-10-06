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
# We have a 4x4 matrix X
# We want an alias A, of length 4, such that
# A[i] <- i * X[i, i+1]
# A[4] <- 4

p <- lp_problem() |> 
    lp_variable(
        X[1:4, 1:4]
    ) |> 
    lp_alias_manual(
        A[1:4], {
            for (i in 1:3) A[i] <- i * X[i, i+1]
            A[4] <- 4
        }
    )
```
