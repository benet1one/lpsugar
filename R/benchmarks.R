
transportation_benchmark <- function(n_warehouses = 50, n_markets = 20) {
    rlang::check_installed("withr")
    
    n <- n_warehouses
    m <- n_markets
    
    W <- 1:n
    M <- paste0("Market-", 1:m)
    
    withr::with_seed(123, {
        cost <- stats::runif(n*m)
        stock <- stats::runif(n)
        demand <- stats::runif(m) |> parameter(M)
        stock <- stock / sum(stock)
        demand <- 0.9 * demand / sum(demand)
    })
    
    # Happy checker
    x <- NULL
    built <- NULL
    
    lp_problem() |> 
        lp_var(x[W, M], lower = 0) |> 
        lp_var(built[W], binary = TRUE) |> 
        lp_min(sum_over(x[i, j], i=W, j=M)) |> 
        lp_con(
            for (i in W) sum(x[i, ]) <= stock[i] * built[i],
            for (j in M) sum(x[, j]) <= demand[j],
        )
}
