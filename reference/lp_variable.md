# Define a variable for an [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)

Add a variable to a problem by defining it's name, dimensions, type, and
bounds.

## Usage

``` r
lp_variable(
  .problem,
  definition,
  integer = FALSE,
  binary = FALSE,
  lower = -Inf,
  upper = +Inf
)

lp_var(
  .problem,
  definition,
  integer = FALSE,
  binary = FALSE,
  lower = -Inf,
  upper = +Inf
)
```

## Arguments

- .problem:

  An
  [`lp_problem()`](https://benet1one.github.io/lpsugar/reference/lp_problem.md)
  object.

- definition:

  Expression.

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

- integer:

  Boolean, whether to treat variable as integer.

- binary:

  Boolean, whether to treat variable as binary, {0, 1}.

- lower:

  Numeric scalar or array. Lower bound for the variable.

- upper:

  Numeric scalar or array. Upper bound for the variable.

## Value

The `.problem` with an added variable in `$variables`. The fields of
`lp_variable` objects are intended for internal use, modifying them is
highly discouraged.

- `$name` : String, name of the variable.

- `$lower` and `$upper` : Bounds.

- `$type` : String, one of `"real"`, `"integer"` or `"binary"`.

- `$integer` and `$binary` : Booleans. If `$binary` is true, then
  `$integer` is also true.

The following fields are meant for internal use only.

- `$ind` : Integer array. Indicates which indices correspond to this
  variable. Meant for internal use only.

- `$coef` : Numeric matrix. The number of rows is the length of the
  variable, the number of columns is the total amount of variables in
  the problem. The values represent coefficients that are modified when
  adding variables or multiplying by a constant. Used for objective,
  constraints and aliases.

- `$add` : Numeric column vector. The number of rows is the length of
  the variable. The values represent addends, modified when adding the
  variable and a constant. Used for objective, constraints and aliases.

- `$raw` : Boolean indicating if the variable has been modified in any
  way (indexed, multiplied, summed, ...) or remains as defined.

## Examples

``` r
# Diet problem
## You can only eat as much as you have of each food.

pantry <- c(Cookies = 8, Bread = 5, Milk = 1, Cereal = 3)
food <- names(pantry)

lp_problem() |>
    lp_variable(diet[food], lower = 0, upper = pantry)
#> <lpsugar Linear Problem>
#> 
#> -- $variables --
#> $diet
#> Real variable 'diet[food]'
#> 
#> 


# Graph problem
## active[i,j] = 1  if the edge from i to j is active.

nodes <- paste("node", 1:10)

lp_problem() |>
    lp_variable(active[nodes, nodes], binary = TRUE)
#> <lpsugar Linear Problem>
#> 
#> -- $variables --
#> $active
#> Binary variable 'active[nodes, nodes]'
#> 0 <= active <= 1
#> 
#> 
```
