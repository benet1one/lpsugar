
test_that("printing", {
    withr::local_package("ROI")
    p <- lp_problem() |>
        lp_variable(x, lower = 0) |>
        lp_variable(y, lower = 0, integer = TRUE) |>
        lp_alias(two_x = 2*x) |>
        lp_maximize(x + y) |>
        lp_constraint(
            x + 2*y <= 10,
            2*x + y <= 10
        )

    s <- lp_solve(p)
    expect_snapshot(p)
    expect_snapshot(s)
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
    len1set <- c("s")

    p <- lp_problem() |>
        lp_variable(x[r, c], binary = TRUE) |>
        lp_variable(y[len1set]) |>
        lp_alias(total = sum(x)) |>
        lp_maximize(total + 1)

    s <- lp_solve(p)

    expect_equal(s$status$code, 0)
    expect_equal(s$objective, s$aliases$total + 1)

    expect_equal(
        dimnames(s$variables$x),
        list(r = r, c = c)
    )

    expect_equal(
        dimnames(s$variables$y),
        list("len1set" = "s")
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

test_that("infeasible", {
    withr::local_package("ROI")
    p <- lp_problem() |>
        lp_variable(z[1:3]) |>
        lp_alias(a = 2*z[1]) |>
        lp_constraint(z <= z - 1)

    s <- lp_find_feasible(p)

    expect_equal(
        dimnames(p$variables$z),
        dimnames(s$variables$z)
    )
})

test_that("binary bounds", {
    withr::local_package("ROI")
    set <- letters[1:2]
    l <- c(0, 0.7) |> parameter(set)
    u <- c(
        0.8, 1,
        1, 1
    ) |> parameter(set, set)

    s <- lp_problem() |>
        lp_var(x[set, set], binary = TRUE, upper = u) |>
        lp_var(y[set], binary = TRUE, lower = l) |>
        lp_max(sum(x) - sum(y)) |>
        lp_solve("highs")

    expect_equal(s$variables$x, floor(u), ignore_attr = TRUE)
    expect_equal(s$variables$y, ceiling(l), ignore_attr = TRUE)
})

test_that("no applicable solver", {
    pkgload::unload("ROI")
    pkgload::unload("ROI.plugin.highs")

    p <- lp_problem() |>
        lp_var(x, lower = 0) |>
        lp_min(x)

    expect_error(
        lp_solve(p),
        "No applicable solver"
    )
})

