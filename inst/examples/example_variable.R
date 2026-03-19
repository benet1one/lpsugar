# Diet problem
## You can only eat as much as you have of each food.

pantry <- c(Cookies = 8, Bread = 5, Milk = 1, Cereal = 3)
food <- names(pantry)

lp_problem() |>
    lp_variable(diet[food], lower = 0, upper = pantry)


# Graph problem
## active[i,j] = 1  if the edge from i to j is active.

nodes <- paste("node", 1:10)

lp_problem() |>
    lp_variable(active[nodes, nodes], binary = TRUE)

