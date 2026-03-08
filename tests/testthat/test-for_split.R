
a <- letters[1:3]

p <- lp_problem() |>
    lp_variable(x, integer = TRUE) |>
    lp_variable(y[a], binary = TRUE) |>
    lp_variable(z[1:2, a], lower = 0)


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

q4 <- rlang::quo({
    for (i in 1:3) {
        hey
        for (j in 1:2) {
            i + j
        }
    }
})

q5 <- rlang::quo(
    for (i in 1:3) for (j in i:3) {
        i + j
    }
)

for_split(q1)
for_split(q2)
for_split(q5) |> purrr::list_flatten(name_spec = "{outer}{inner}")

for_split(q1, evaluate = TRUE, data = data_mask(p))
for_split(q2, evaluate = TRUE, data = data_mask(p))
for_split(q5, evaluate = TRUE, data = data_mask(p))


test_that("for_split", {
    expect_identical(for_split(q2), for_split(q3))
    expect_identical(
        for_split(q2, evaluate = TRUE, data = data_mask(p)),
        for_split(q3, evaluate = TRUE, data = data_mask(p))
    )
})
