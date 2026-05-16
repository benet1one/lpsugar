# Add constraints to an [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)

Restrict the variables in an
[`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)
with linear constraints.

## Usage

``` r
lp_constraint(.problem, ...)

lp_con(.problem, ...)

lp_subject_to(.problem, ...)
```

## Arguments

- .problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

- ...:

  One or more linear constraints. Can be named. They must:

  - Contain one or more variables defined with
    [`lp_variable()`](https://benet1one.github.io/lpsugar/reference/lp_variable.md)

  - Contain a comparison operator ( `< / <= / == / => / >` )

## Value

The `.problem` with added `$constraints`. (Note: previous constraints
are not overritten).

Constraints can be represented as `lhs * vars <dir> rhs`.

The `$constraints` field has the following subfields:

- `$lhs` :
  [`slam::simple_triplet_matrix()`](https://rdrr.io/pkg/slam/man/matrix.html)
  where each row is a constraint, each column is a variable, and the
  values represent coefficients.

- `$dir` : Character vector with elements `"<="`, `"=="`, or `">="`, the
  direction of each constraint.

- `$rhs` : Numeric column vector representing the right hand side of
  each constraint.

- `$name` : Character vector with the names of the constraints, if `...`
  is named, or `""` for unnamed constraints.

- `$call` : Expression that defined each constraint.

## Examples

``` r
# Ordered variable constraint
## ordered[i] > ordered[i-1]  for all i in 2:n
n <- 4
p <- lp_problem() |>
    lp_variable(ordered[1:n], lower = 0)

## Three alternatives
pc <- p |> lp_constraint(
    alt1 = ordered[2:n] > ordered[1:(n-1)],
    alt2 = for (i in 2:n) ordered[i] > ordered[i-1]
)

## The 'for' construct can be wrapped around 'lp_constraint' instead of inside it
for (i in 2:n) {
    pc <- pc |> lp_constraint(
        alt3 = ordered[i] > ordered[i-1]
    )
}

## The only difference are the rownames of the constraint matrix
print(pc$constraints)
#> 
#>  alt1 | n = 3 | ordered[2:n] > ordered[1:(n - 1)]
#> 
#>      ordered[1] ordered[2] ordered[3] ordered[4] dir  
#> alt1 -1         1          0          0          >=  0
#> alt1 0          -1         1          0          >=  0
#> alt1 0          0          -1         1          >=  0
#> 
#> 
#>  alt2 | n = 3 | for (i in 2:n) ordered[i] > ordered[i - 1]
#> 
#>           ordered[1] ordered[2] ordered[3] ordered[4] dir  
#> alt2[i=2] -1         1          0          0          >=  0
#> alt2[i=3] 0          -1         1          0          >=  0
#> alt2[i=4] 0          0          -1         1          >=  0
#> 
#> 
#>  alt3 | n = 3 | ordered[i] > ordered[i - 1]
#> 
#>      ordered[1] ordered[2] ordered[3] ordered[4] dir  
#> alt3 -1         1          0          0          >=  0
#> alt3 0          -1         1          0          >=  0
#> alt3 0          0          -1         1          >=  0
#> 
```
