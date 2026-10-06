# Changelog

## lpsugar 0.28.0

### New Features

- New function
  [`lp_alias_manual()`](https://benet1one.github.io/lpsugar/reference/lp_alias_manual.md)
  which provides a much more powerful syntax for defining aliases.

## lpsugar 0.27.2

### Performance Improvements

- Improved performance when binding many linear constraints.

### Bug Fixes

- Fixed bug with indexing variables.

## lpsugar 0.27.1

### Performance Improvements

- Slightly improved performance when programatically adding constraints.

## lpsugar 0.27.0

### New Features

- New argument `fixed` in `lp_variable` allows the user to fix part of
  the variable to certain constant values.

### Performance Improvements

- Problems where many variables are fixed are now faster and lighter.

## lpsugar 0.26.2

### Bug Fixes

- Incorrect error message when dimensions of variable differ from
  dimensions of its bounds.

## lpsugar 0.26.1

### Bug Fixes

- Fixed bug with updating objective after adding a new variable.

## lpsugar 0.26.0

### Breaking Changes

- Replaced `lp_minimize_function()` with a different
  [`nonlinear()`](https://benet1one.github.io/lpsugar/reference/nonlinear.md)
  workflow.

- The `$objective` function and the `$constraints` now inherit from
  ROI’s `objective` and `constraint` classes, respectively.

## lpsugar 0.25.1

### New Features

- [`ROI::solution()`](https://rdrr.io/pkg/ROI/man/solution.html) now has
  a method for class `<lp_solution>`
- Cleaner constraint printing for constraints wrapped in curly brackets
  [`{}`](https://rdrr.io/r/base/Paren.html)

## lpsugar 0.25.0

### Breaking Changes

- Matrix Multiplication no longer drops dimensions of arguments

### New Features

- Implemented Matrix Multiplication between a quadratic variable and a
  numeric matrix

### Bug Fixes

- Matrix Multiplication transforms row vectors into column vectors

## lpsugar 0.24.0

### Breaking Changes

Notation is now more consistent with package `ROI`

- `q_coef` and `q_lhs` are now `Q`
- `coef` and `lhs` are now `L`
- `add` is now `A`

### Minor Improvements

- Added subclasses to errors
- Commented Code

## lpsugar 0.23.5

- Cleaned up code and documentation.
