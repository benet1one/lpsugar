# Add Constraints to an [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)

Restrict the variables in an
[`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)
with linear or quadratic constraints.

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

  - Contain a comparison operator, such as `<=`, `==` or `=>`.

  - If it's a nonlinear constraint, it must be written as:

    [`nonlinear()`](https://benet1one.github.io/lpsugar/reference/nonlinear.md)` <= number`

## Value

The `.problem` with added `$constraints`. Previous constraints are not
overwritten, so it's possible to call `lp_constraint()` multiple times
without overwriting previously defined constraints.

The `$constraints` inherit from
[`ROI::L_constraint()`](https://rdrr.io/pkg/ROI/man/L_constraint.html),
[`ROI::Q_constraint()`](https://rdrr.io/pkg/ROI/man/Q_constraint.html)
or
[`ROI::F_constraint()`](https://rdrr.io/pkg/ROI/man/F_constraint.html).

- Quadratic constraints are represented as

  \\\frac{1}{2} x'Q\_{i}x + L\_{i}x \le \text{rhs}\_{i} \qquad \forall
  i\\

- While nonlinear constraints are represented as

  \\F(x)\_i \le \text{rhs}\_i \qquad \forall i\\

## Examples

``` r
# Order Constraint ------------------------------
## ordered[i] > ordered[i-1]  forall i in 2:n
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

## The only difference are the row names of the constraint matrix when printing
print(pc$constraints)
#> An object containing 9 linear constraints.
#> 
#> alt1
#> | ordered[2:n] > ordered[1:(n - 1)]
#> | Rows = 3
#> 
#>        ordered[1] ordered[2] ordered[3] ordered[4] dir rhs
#>   alt1 -1         1          0          0          >=  0  
#>   alt1 0          -1         1          0          >=  0  
#>   alt1 0          0          -1         1          >=  0  
#> 
#> alt2
#> | for (i in 2:n) ordered[i] > ordered[i - 1]
#> | Rows = 3
#> 
#>             ordered[1] ordered[2] ordered[3] ordered[4] dir rhs
#>   alt2[i=2] -1         1          0          0          >=  0  
#>   alt2[i=3] 0          -1         1          0          >=  0  
#>   alt2[i=4] 0          0          -1         1          >=  0  
#> 
#> alt3
#> | ordered[i] > ordered[i - 1]
#> | Rows = 3
#> 
#>        ordered[1] ordered[2] ordered[3] ordered[4] dir rhs
#>   alt3 -1         1          0          0          >=  0  
#>   alt3 0          -1         1          0          >=  0  
#>   alt3 0          0          -1         1          >=  0  
#> 


# Nonlinear Constraint --------------------------
## log(x + 1) > y  =>  log(x + 1) - y > 0
nlp <- lp_problem() |> 
    lp_variable(x, lower = 0) |> 
    lp_variable(y) |> 
    lp_constraint(
        # Nonlinear constraints must always be written as `nonlinear(...) <= number`
        nl_con = nonlinear(log(x + 1) - y) > 0
    )

print(nlp$constraints)
#> An object containing 1 nonlinear constraint.
#> 
#> nl_con
#> | nonlinear(log(x + 1) - y) > 0
#> | Rows = 1
#> 
```
