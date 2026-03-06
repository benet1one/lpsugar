
#' @export
lp_problem <- function() {
    list(
        variables = list(),
        objective = numeric(),
        constraints = new_constraints(),

        .nvar = 0L,
        .varnames = character()

    ) |> structure(class = "lp_problem")
}

new_constraints <- function() {
    list()
}
