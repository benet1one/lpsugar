
test_that("variable definitions", {
    a <- letters[1:3]

    expect_snapshot(
        parse_variable_definition(t[a, b = 1:5])
    )
    expect_snapshot(
        parse_variable_definition(t[b = a, 1:5])
    )
    expect_equal(
        parse_variable_definition(ayo),
        parse_variable_definition("ayo")
    )
    expect_equal(
        parse_variable_definition(x[a]),
        parse_variable_definition("x"[a])
    )
})

test_that("variable definition errors", {
    expect_error(
        lp_problem() |> lp_variable(x[b])
    )
    expect_error(
        lp_problem() |> lp_variable()
    )
    expect_error(
        lp_problem() |> lp_variable(x[, 1:3]),
        "cannot be missing."
    )
    expect_error(
        lp_problem() |> lp_variable(x[1:3, ]),
        "cannot be missing."
    )
    expect_error(
        lp_problem() |> lp_variable(mean(1)),
        "Failed to parse variable"
    )
    expect_error(
        lp_problem() |> lp_variable(mean()[1]),
        "Failed to parse variable"
    )

    expect_error(
        lp_problem() |> lp_variable(x) |> lp_variable(x[1:5]),
        "already exists"
    )
})

test_that("variable bounds", {
    expect_error(
        lp_problem() |> lp_variable(x, lower = 2, upper = 1),
        "cannot be greater than `upper` bound"
    )
    expect_error(
        lp_problem() |> lp_variable(x, lower = +Inf),
        "cannot be \\+Inf"
    )
    expect_error(
        lp_problem() |> lp_variable(x, upper = -Inf),
        "cannot be \\-Inf"
    )

    expect_warning(
        p1 <- lp_problem() |> lp_variable(x, lower = NULL),
        "`NULL` or zero-length, setting to -Inf"
    )
    expect_equal(p1$variables$x$lower, -Inf)


    expect_warning(
        p2 <- lp_problem() |> lp_variable(x[1:3], upper = c(2, NA, 1)),
        "containts NA values, setting to Inf"
    )
    expect_equal(p2$variables$x$upper, c(2, +Inf, 1))

    expect_error(
        lp_problem() |> lp_variable(x, upper = "3"),
        "not numeric"
    )

    expect_error(
        lp_problem() |> lp_variable(x[1:2, 1:3], lower = matrix(0, nrow = 3, ncol = 2)),
        "`dim\\(lower\\)` different"
    )
})

test_that("variable concatenation", {
    p <- lp_problem() |>
        lp_var(x[1:3]) |>
        lp_var(y[1:2, 1:2])

    x <- p$variables$x
    y <- p$variables$y

    expect_error(
        c(x, 2),
        "Use `bind_vars\\(\\)` instead."
    )
    expect_warning(
        bind_vars(x, a_name = 2),
        "ignoring named arguments"
    )
    expect_snapshot(
        bind_vars(1:2, y, x[1], 3)
    )
    expect_equal(
        bind_vars(1, 4:5),
        c(1, 4:5)
    )
    expect_equal(
        bind_vars(1, y, x[1:2]) $ ind,
        1:7,
        ignore_attr = TRUE
    )

    expect_equal(
        bind_vars(),
        numeric(0)
    )
    expect_equal(
        bind_vars(integer(0), NULL),
        numeric(0)
    )
    expect_equal(
        bind_vars(, x, NULL, integer(0), y, , 1, ),
        bind_vars(x, y, 1)
    )
    expect_equal(
        bind_vars(x, y[integer()]),
        x
    )
})

test_that("variable indexing", {
    a <- letters[1:3]
    p <- problem_variables()
    x <- p$variables$x
    y <- p$variables$y
    z <- p$variables$z

    expect_identical(y[1:2], y[-3])
    expect_identical(y[1:2], y[c(TRUE, TRUE, FALSE)])
    expect_identical(y[1:2], y[c("a", "b")])
    expect_identical(rev(y), y[3:1])
    expect_identical(rev(y), y[rev(a)])

    expect_equal(
        rep(x, 3) + y,
        x + y
    )

    expect_equal(
        rep(y, 2),
        rep(y, length.out = 6)
    )

    expect_equal(
        rep(y, each = 2),
        y[c(1, 1, 2, 2, 3, 3)]
    )

    z1 <- z[1, ] $ ind
    z1_dropped <- z[1, , drop = TRUE] $ ind

    expect_all_true(dim(z1) == c(1, 3))
    expect_true(is.null(dim(z1_dropped)))

    expect_error(y[4], "out of bounds")
    expect_error(y[-4], "out of bounds")
    expect_error(y[0], "Invalid subscript")
    expect_error(y["d"], "Invalid subscript 'd'")
    expect_error(y[1,1], "Incorrect number of dimensions")

    # Transpose
    expect_identical(z$coef, t(t(z))$coef)
    expect_snapshot(t(z))
    expect_equal(
        dim(t(y)),
        c(1, 3)
    )

    p2 <- lp_problem() |> lp_variable(ThreeD[1:2, 1:3, 1:3])
    ThreeD <- p2$variables$ThreeD

    expect_error(
        t(ThreeD[, , 1]),
        "Index it with `ThreeD"
    )
    expect_snapshot(
        t(ThreeD[, , 1, drop = TRUE])
    )

    # Other errors
    expect_error(
        y[1] <- 2,
        "Cannot assign an element of an `lp_variable`"
    )
    expect_error(
        y[[1]],
        "Double indexing `y\\[\\[i\\]\\]` not supported for `lp_variable`"
    )
    expect_error(
        y[[1]] <- 2,
        "Double indexing `y\\[\\[i\\]\\]` not supported for `lp_variable`"
    )
})

test_that("operations", {
    a <- letters[1:3]
    p <- problem_variables()
    x <- p$variables$x
    y <- p$variables$y
    z <- p$variables$z

    expect_identical(x, +x)
    expect_identical(x/2, x*0.5)
    expect_identical(1-y, !y)
    expect_identical(1-y[1], !y[1])
    expect_true( (!y)$binary )

    expect_identical(2*y, y + y)
    expect_identical(-z, z - 2*z)

    expect_error(c(1, 2, NA) + y, "Left-hand-side object contains NA values")
    expect_error(y / c(1, 2, NA), "Right-hand-side object contains NA values")

    expect_error(2/x, "Cannot divide by a variable")
    expect_error(!x, "only supported for binary variables")

    expect_error(!(y[1] + x), "only supported for binary variables")
    expect_error(!(x + y[1]), "only supported for binary variables")

    expect_error(abs(x), "absolute value")
    expect_error(exp(x), "exp")

    expect_error(y | y, "Unsupported operation")
    expect_error(y & y, "Unsupported operation")
    expect_error(y %% y, "Unsupported operation")
    expect_error(y %/% y, "Unsupported operation")
    expect_error(
        x & sum({
            k <- 2
            k*y
        }),
        "Unsupported operation"
    )

    expect_error(y + z, "non-conformable")
    expect_error(y * 1:2, "Non-conformable")
    expect_error(z + t(z), "non-conformable")

    p2 <- lp_problem() |>
        lp_variable(u[1:2, 1:2, 1:3]) |>
        lp_variable(t[1:2, 1:3, 1:2])


    u <- p2$variables$u
    t <- p2$variables$t
    b <- array(5, c(2, 2, 3))

    u[1,,] + u[,1,]
    u[,1,1] + u[1,,1]
    u + b

    u[,,1] - t[,1,]
    u[1,,1] - t[,1,1]
    u[1,1,] - t[1,,1]

    expect_error(u[1,,] + t[1,,], "non-conformable")
    expect_error(t + b, "non-conformable")

    expect_snapshot(diff(y) $ coef)
    expect_snapshot(diff(y, lag = 2) $ coef)
    expect_equal(
        diff(z, differences = 2),
        diff(diff(z))
    )
    expect_equal(
        diff(z, lag = 2, differences = 2),
        z |> diff(lag = 2) |> diff(lag = 2)
    )
})

test_that("sum", {
    p <- problem_variables()
    x <- p$variables$x
    y <- p$variables$y
    z <- p$variables$z

    expect_identical(
        sum(x, y, 2*z, 1:4),
        x + y[1] + y[2] + y[3] + 2*sum(z) + sum(1:4)
    )
    expect_identical(
        sum(z[1:5]) $ coef,
        cumsum(z)[5] $ coef
    )

    expect_no_error(
        sum(y, c(1, 1, NA), na.rm = TRUE)
    )
    expect_error(
        sum(y, c(1, 1, NA), na.rm = FALSE) # Default
    )
})

test_that("matrix multiply with vector", {
    r <- 1:2
    c <- 1:3

    beta <- c(3, 2, -1)

    set.seed(14232)
    xval <- rpois(length(r) * length(c), 7) |> parameter(r, c)

    p <- lp_problem() |>
        lp_variable(x[r, c]) |>
        lp_alias(
            xb = x %*% beta,
            bx = t(beta) %*% t(x)
        )

    xb <- p$aliases$xb
    bx <- p$aliases$bx

    expect_equal(
        xb$ind,
        t(bx)$ind
    )
    expect_equal(
        xb$coef,
        t(bx)$coef
    )

    computed <- compute_aliases(p, list(x = xval))

    expect_equal(
        computed$xb,
        xval %*% beta
    )

    expect_equal(
        computed$xb,
        t(computed$bx)
    )

    expect_equal(
        p |> lp_eval(beta %*% matrix(2)),
        beta %*% matrix(2)
    )

    expect_error(
        p |> lp_eval(beta %*% x),
        "non-conformable"
    )
})

test_that("matrix multiply with matrix", {
    a <- 1:2
    b <- 1:3

    gamma <- rpois(length(a) * length(b), 7) |> parameter(a, b)
    mval <- rpois(length(a) * length(b), 5) |> parameter(a, b)
    vval <- c(4, -3, 7)

    p <- lp_problem() |>
        lp_variable(v[b]) |>
        lp_variable(m[a, b]) |>
        lp_alias(
            gv = gamma %*% v,
            vg = t(v) %*% t(gamma),
            mg = t(m) %*% gamma,
            gm = m %*% t(gamma)
        )

    computed <- compute_aliases(p, list(v = vval, m = mval))

    expect_equal(
        computed$gv,
        gamma %*% vval
    )
    expect_equal(
        computed$vg,
        t(vval) %*% t(gamma)
    )
    expect_equal(
        computed$mg,
        t(mval) %*% gamma
    )
    expect_equal(
        computed$gm,
        mval %*% t(gamma)
    )
})

test_that("quadratic matrix multiply", {
    a <- 1:2
    b <- 1:3

    xval <- rpois(length(a) * length(b), 8) |> parameter(a, b)

    p <- lp_problem() |>
        lp_variable(x[a, b]) |>
        lp_alias(
            cprod = t(x) %*% x,
            tcprod = x %*% t(x)
        )

    computed <- compute_aliases(p, list(x = xval))

    expect_equal(
        computed$cprod,
        crossprod(xval)
    )
    expect_equal(
        computed$tcprod,
        tcrossprod(xval)
    )
})

