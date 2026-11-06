# Print method for constraints.

Print method for constraints.

## Usage

``` r
# S3 method for class 'lp_constraint'
print(x, full = ncol(x$L) <= 20, ...)
```

## Arguments

- x:

  An
  [`lp_constraint()`](https://benet1one.github.io/lpsugar/reference/lp_constraint.md).

- full:

  Boolean. If TRUE, linear constraints with less than 200 rows are
  printed fully. If FALSE, only prints a summary of each constraint.

- ...:

  Ignored.
