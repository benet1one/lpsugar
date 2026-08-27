# Order Constraint ------------------------------
## ordered[i] > ordered[i-1]  forall i in 2:n
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

## The only difference are the row names of the constraint matrix when printing
print(pc$constraints)


# Nonlinear Constraint --------------------------
## log(x + 1) > y  =>  log(x + 1) - y > 0
nlp <- lp_problem() |> 
    lp_variable(x, lower = 0) |> 
    lp_variable(y) |> 
    lp_constraint(
        # Nonlinear constraints must always be written as `nonlinear(...) <= number`
        nl_con = nonlinear(log(x + 1) - y) > 0
    )

print(nlp$constraints)
