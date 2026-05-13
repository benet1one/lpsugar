
test_that("for_split works", {
    a <- 1:2
    b <- letters[1:3]

    p <- lp_problem() |>
        lp_var(x[a, b])

    q1 <- rlang::quo({
        for (i in 1:3) {
            i^2
        }
    })

    for_split(q1) |> expect_snapshot()

    q2 <- rlang::quo(
        for (i in a) for (j in b) {
            x[i, j] + i
        }
    )

    for_split(q2, data = data_mask(p)) |> expect_snapshot()
})

test_that("for_split with interruption", {
    q <- rlang::quo(for (i in 1:4) {
        if (i == 3) {
            next
        }
        2*i
    })

    expect_equal(
        for_split(q),
        list(
            "i=1" = 2,
            "i=2" = 4,
            "i=4" = 8
        )
    )
})

test_that("advanced for_split", {
    q_advanced <- rlang::quo(for (i in 1:4) {
        k <- i + 1

        for (j in k:(k + 1)) if (k %% 2 == 0) {
            c(i = i, j = j, k = k)
        }
    })

    for_split(q_advanced) |> expect_snapshot()
})
