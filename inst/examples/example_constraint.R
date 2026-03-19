# Ordered variable constraint
## ordered[i] > ordered[i-1]  for all i in 2:n
n <- 4
p <- lp_problem() |>
    lp_variable(ordered[1:n], lower = 0)

## Three alternatives
pc <- p |> lp_constraint(
    alt1 = ordered[2:n] > ordered[1:(n-1)],
    alt2 = for (i in 2:n) ordered[i] > ordered[i-1]
)

## The 'for' construct can be wrapped around 'lp_constraint' instead of inside it
for (i in 2:n) {
    pc <- pc |> lp_constraint(
        alt3 = ordered[i] > ordered[i-1]
    )
}

## The only difference are the rownames of the constraint matrix
print(pc$constraints)
