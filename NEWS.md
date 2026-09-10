# lpsugar 0.26.1

## Bug Fixes

* Fixed bug with updating objective after adding a new variable.

# lpsugar 0.26.0

## Breaking Changes

* Replaced `lp_minimize_function()` with 
a different `nonlinear()` workflow.

* The `$objective` function and the `$constraints`
now inherit from ROI's `objective` and `constraint`
classes, respectively.

# lpsugar 0.25.1

## New Features

* `ROI::solution()` now has a method for class `<lp_solution>`
* Cleaner constraint printing for constraints wrapped in curly brackets `{}`

# lpsugar 0.25.0

## Breaking Changes

* Matrix Multiplication no longer drops dimensions of
arguments

## New Features

* Implemented Matrix Multiplication between a quadratic
variable and a numeric matrix

## Bug Fixes

* Matrix Multiplication transforms row vectors into column
vectors

# lpsugar 0.24.0

## Breaking Changes

Notation is now more consistent with package `ROI`

* `q_coef` and `q_lhs` are now `Q`
* `coef` and `lhs` are now `L`
* `add` is now `A`

## Minor Improvements

* Added subclasses to errors
* Commented Code

# lpsugar 0.23.5

* Cleaned up code and documentation.
