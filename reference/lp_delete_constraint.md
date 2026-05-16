# Delete constraints

Remove named constraints from an
[`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

## Usage

``` r
lp_delete_constraint(.problem, names)
```

## Arguments

- .problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md).

- names:

  Characted vector with the names of constraints to be deleted. It is
  not possible to delete unnamed constraints, so make sure to name them
  if you plan to delete them later.

## Examples

``` r
p <- lp_problem() |>
    lp_variable(x) |>
    lp_constraint(
        c1 = x > 0,
        c2 = x < 10
    )

some_condition <- TRUE
if (some_condition) {
    p <- p |> lp_delete_constraint("c1")
}

print(p)
#> <lpsugar Linear Problem>
#> 
#> -- $variables --
#> $x
#> Real scalar 'x'
#> 
#> -- $constraints --
#>  c2 | n = 1 | x < 10
```
