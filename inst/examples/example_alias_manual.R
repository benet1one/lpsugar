# We have a 4x4 matrix X
# We want an alias A, of length 4, such that
# A[i] <- i * X[i, i+1]
# A[4] <- 4

p <- lp_problem() |> 
    lp_variable(
        X[1:4, 1:4]
    ) |> 
    lp_alias_manual(
        A[1:4], {
            for (i in 1:3) A[i] <- i * X[i, i+1]
            A[4] <- 4
        }
    )
