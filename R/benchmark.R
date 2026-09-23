
linear_benchmark <- function() {
    np <- 50
    people <- 1:np
    affinity <- stats::runif(np^2) |> parameter(people, people)
    
    affinity[lower.tri(affinity)] <- 0
    diag(affinity) <- 0
    
    upper_bound <- upper.tri(affinity)
    upper_bound[] <- as.numeric(upper_bound)
    
    lp_problem() |> 
        lp_variable(match[people, people], binary = TRUE, upper = upper_bound) |> 
        lp_maximize(sum(affinity * match)) |> 
        lp_constraint(
            for (p in people) sum(match[p, ]) + sum(match[, p]) == 1
        )
}
