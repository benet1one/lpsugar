
test_that("for_split works", {
    p <- problem_variables()

    q1 <- rlang::quo({
        for (i in 1:5) {
            y * i^2
        }
    })

    q2 <- rlang::quo({
        for (i in 1:3) {
            for (j in 1:2) {
                k <- i + j
                i*x + k
            }
        }
    })

    q3 <- rlang::quo({
        for (i in 1:3) for (j in 1:2) {
            k <- i + j
            i*x + k
        }
    })

    for_split(q1)
    expect_snapshot(for_split(q1, evaluate = TRUE, data = data_mask(p)))

    expect_identical(for_split(q2), for_split(q3))
    expect_identical(
        for_split(q2, evaluate = TRUE, data = data_mask(p)),
        for_split(q3, evaluate = TRUE, data = data_mask(p))
    )

    expect_equal(
        for_split(rlang::quo(1 + 1), evaluate = TRUE),
        2
    )

    q4 <- rlang::quo({
        k <- 3
        k*x - k
    })

    expect_identical(
        for_split(q4, evaluate = TRUE, data = data_mask(p)),
        3 * p$variables$x - 3
    )
})
