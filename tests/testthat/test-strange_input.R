
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
