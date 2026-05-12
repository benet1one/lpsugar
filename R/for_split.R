
check_for_split <- function(quosure, call = parent.frame()) {
    nams <- all.names(quosure)

    if ("return" %in% nams) {
        abort("Cannot use `return` inside a for loop.", call = call)
    }
}

for_split <- function(quosure, data = NULL) {
    check_for_split(quosure, call = parent.frame())
    expr <- rlang::get_expr(quosure) |> inside()
    env <- rlang::get_env(quosure)

    if (!is_loop(expr)) {
        return(list(rlang::eval_tidy(quosure, data = data)))
    }

    expr <- include_all_element_adders(expr)
    ind_names <- get_loop_indices(expr)
    result_env <- rlang::new_environment(parent = env)

    with(result_env, {
        .___result <- base::list()
        .___indices <- base::list()
        .___add_loop_element <- function(x) {
            ind <- as.list(parent.frame())
            x # prevents execution when there's a `next`

            .___indices <<- base::c(
                .___indices,
                base::list(ind)
            )

            .___result <<- base::c(
                .___result,
                base::list(x)
            )
        }
    })

    rlang::eval_tidy(expr, data = data, env = result_env)
    result <- result_env$.___result

    names(result) <- sapply(result_env$.___indices, function(i) {
        i <- i[names(i) %in% ind_names]
        val <- sapply(i, format_ind)
        paste0(names(i), "=", val, collapse = ", ")
    })

    result
}

is_assignment <- function(expr) {
    rlang::is_call(expr) && (
        expr[[1]] == quote(`<-`) ||
            expr[[1]] == quote(`=`)
    )
}

is_loop <- function(expr) {
    rlang::is_call(expr) && identical(expr[[1]], quote(`for`))
}

contains_loop <- function(expr) {
    if (!rlang::is_call(expr)) {
        return(FALSE)
    }
    if (is_loop(expr)) {
        return(TRUE)
    }
    for (e in as.list(expr)) if (!rlang::is_missing(e) && contains_loop(e)) {
        return(TRUE)
    }

    FALSE
}

include_all_element_adders <- function(expr) {
    if (!contains_loop(expr)) {
        return(expr)
    }

    if (!is_loop(expr)) {
        expr[] <- lapply(as.list(expr), include_all_element_adders)
        return(expr)
    }

    if (!contains_loop(expr[-1])) {
        return(include_element_adder(expr))
    }

    expr[-1] <- lapply(as.list(expr)[-1], include_all_element_adders)
    return(expr)
}

include_element_adder <- function(expr) {
    last <- length(expr)
    e <- expr[[last]]

    if (is_assignment(e)) {
        return(expr)
    }

    expr[[last]] <- rlang::expr(
        .___add_loop_element(!!e)
    )

    expr
}

get_loop_indices <- function(expr) {
    if (!rlang::is_call(expr) || !contains_loop(expr)) {
        return(character(0))
    }

    if (is_loop(expr)) {
        here_index <- c(expr[[2]] |> format())
    } else {
        here_index <- character(0)
    }

    inside_indices <- as.list(expr) |>
        lapply(get_loop_indices) |>
        unlist()

    c(here_index, inside_indices)
}

format_ind <- function(i) {
    if (rlang::is_integer(i, n = 1)) {
        as.character(i)
    } else {
        rlang::as_label(i)
    }
}
