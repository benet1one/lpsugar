
x <- matrix(0, nrow = 2, ncol = 3) |> robust_index()
colnames(x) <- letters[1:3]

x[, "a"]
