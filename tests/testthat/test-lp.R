
test_that("printing", {
    lp_problem() |>
        lp_variable(x, lower = 0) |>
        lp_variable(y, lower = 0, integer = TRUE) |>
        lp_maximize(x + y) |>
        lp_constraint(
            x + 2*y <= 10,
            2*x + y <= 10
        ) |>
        print()
})

test_that("solving with multivariate bounds", {
    withr::local_package("ROI")
    s <- lp_problem() |>
        lp_variable(x[1:2, 1:2], lower = matrix(1:4, 2, 2), upper = 10) |>
        lp_minimize(x[1] + x[2] + x[3] - x[4]) |>
        lp_solve()

    expect_equal(
        s$variables$x,
        matrix(c(1, 2, 3, 10), 2, 2),
        ignore_attr = TRUE
    )
})

test_that("feasible", {
    withr::local_package("ROI")
    no_obj <- lp_problem() |>
        lp_variable(x, lower = 5, upper = 10)

    expect_error(
        lp_solve(no_obj),
        "Did you forget to set the objective function"
    )

    s <- no_obj |> lp_minimize(0) |> lp_solve()
    f <- no_obj |> lp_find_feasible()

    expect_equal(s, f)
})

test_that("no variables", {
    expect_error(
        lp_problem() |> lp_min(0) |> lp_solve(),
        "no variables"
    )
})

test_that("pretty solution optimal", {
    withr::local_package("ROI")
    r <- letters[1:2]
    c <- LETTERS[1:3]

    p <- lp_problem() |>
        lp_variable(x[r, c], binary = TRUE) |>
        lp_alias(total = sum(x)) |>
        lp_maximize(total + 1)

    s <- lp_solve(p)

    expect_equal(s$status$code, 0)
    expect_equal(s$objective, s$aliases$total + 1)

    expect_equal(
        dimnames(s$variables$x),
        list(r = r, c = c)
    )

    ## Even if the variable is an integer, the storage mode needs to be double
    ## so it supports infinity.
    expect_equal(
        storage.mode(s$variables$x),
        "double"
    )

    sl <- lp_solve(p, binary_as_logical = TRUE)

    expect_equal(
        storage.mode(sl$variables$x),
        "logical"
    )
})

test_that("pretty solution unbounded", {
    withr::local_package("ROI")
    p <- lp_problem() |>
        lp_var(y, integer = TRUE) |>
        lp_max(y)
})

test_that("infeasible", {
    withr::local_package("ROI")
    p <- lp_problem() |>
        lp_variable(z[1:3]) |>
        lp_alias(a = 2*z[1]) |>
        lp_constraint(z <= z - 1)

    s <- lp_find_feasible(p)

    expect_true(is.na(s$objective))
    expect_true(is.na(s$aliases$a))
    expect_true( all(is.na(s$variables$z)) )
    expect_equal(
        dimnames(p$variables$z),
        dimnames(s$variables$z)
    )
})

# test_that("timeout", {
#     skip_on_cran()
#
#     p_fast <- lp_problem() |>
#         lp_var(x, lower = 0) |>
#         lp_min(x)
#
#     lp_solve(p_fast, timeout = NULL)
#     lp_solve(p_fast, timeout = 0)
#     lp_solve(p_fast, timeout = -1)
#
#     expect_warning(
#         lp_solve(p_fast, timeout = 0.2),
#         "rounding to 1"
#     )
#
#     expect_error(lp_solve(p_fast, timeout = 2:3))
#     expect_error(lp_solve(p_fast, timeout = "3"))
#
#
#     withr::local_seed(123)
#
#     n <- 250
#     m <- 30
#     objective_coef <- runif(n)
#     constraint_coef <- matrix(
#         rpois(n*m, lambda = 2),
#         nrow = m, ncol = n
#     )
#
#     p_slow <- lp_problem() |>
#         lp_var(x[1:n], lower = 0, upper = 10, integer = TRUE) |>
#         lp_max(sum(x * objective_coef)) |>
#         lp_con(
#             for (i in 1:m)
#                 sum(x * constraint_coef[i, ]) <= 5*n
#         )
#
#     time <- system.time(
#         s_slow <- lp_solve(p_slow, timeout = 1)
#     )
#
#     expect_true(time[1] < 3) # user time
# })
