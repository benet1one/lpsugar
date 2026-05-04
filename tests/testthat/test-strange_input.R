
test_that("splice", {
    a <- alist(a = 2*x, b = a+1)
    c <- alist(
        x >= 5,
        k = for (i in 1:3) { i*x <= 2*i }
    )

    p <- lp_problem() |>
        lp_var(x) |>
        lp_alias(!!!a) |>
        lp_constraint(!!!c)

    expect_true(p$aliases$a$coef == 2)
    expect_true(p$aliases$a$add == 0)

    expect_true(p$aliases$b$coef == 2)
    expect_true(p$aliases$b$add == 1)

    expect_snapshot(p$constraints)
})

test_that("for with no slpit", {
    p <- lp_problem() |>
        lp_var(x) |>
        lp_con(
            mycon = {
                k <- 0
                y <- x
                for (i in 2:4) {
                    k <- k + i
                    y <- y * i
                }
                y <= k
            }
        )

    expect_true(p$constraints$lhs$v == 24)
    expect_true(p$constraints$rhs == 9)
})

test_that("masking", {
    i <- 4
    p1 <- lp_problem() |>
        lp_var(x) |>
        lp_con(for (i in 1:3) x*!!i <= i)

    expect_snapshot(p1$constraints)

    v <- 3
    p2 <- lp_problem() |>
        lp_var(x) |>
        lp_con(for (i in 1:3) {
            v <- 3*i
            x <= v
        })

    expect_snapshot(p2$constraints)
})

test_that("misc", {
    colvec_set <- matrix(1:3, nrow = 3)
    mat_set <- matrix(1:9, nrow = 3)

    lp_problem() |> lp_var(y[colvec_set])

    expect_warning(
        lp_problem() |> lp_var(y[mat_set]),
        "set `mat_set` is 2-dimensional, results may be unexpected"
    )

    # This should not throw an error, I guess...
    lp_problem() |> lp_var( uhh[{hey <- 1:3; hey}] )
})
