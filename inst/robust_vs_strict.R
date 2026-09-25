
x <- matrix(runif(100*200), 100, 200)
colnames(x) <- paste("COL -", 1:200)

xr <- robust_index(x)
xs <- strict_index(x)


bench::mark(
    xr[1:20] |> unclass(),
    xs[1:20] |> unclass()
)
bench::mark(
    xr[1:20, ] |> unclass(),
    xs[1:20, ] |> unclass()
)
bench::mark(
    xr[1:20, 1:50] |> unclass(),
    xs[1:20, 1:50] |> unclass()
)
bench::mark(
    xr[, "COL - 5"] |> unclass(),
    xs[, "COL - 5"] |> unclass()
)
bench::mark(
    xr[, factor("COL - 5")] |> unclass(),
    xs[, factor("COL - 5")] |> unclass()
)
