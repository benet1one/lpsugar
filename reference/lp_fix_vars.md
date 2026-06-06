# Fix Variables to a Value

Set the lower and upper bounds of variables to a fixed value.

## Usage

``` r
lp_fix_vars(.problem, ...)
```

## Arguments

- .problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

- ...:

  Name-value pairs, variables with their respective values. `NA` values
  are not fixed, and will remain free within their bounds.

## Value

The `.problem` with modified lower and upper bounds for the variables.
