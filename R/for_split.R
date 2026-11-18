
# Takes a loop or nested loops and returns a list
# of results. Inspired in comprehenr::to_list().
eval_split_for <- function(quosure, data = NULL) {
    check_for_split(quosure, call = parent.frame())
    expr <- rlang::get_expr(quosure)
    env <- rlang::get_env(quosure)
    
    if (!is_loop(expr)) {
        return(list(rlang::eval_tidy(quosure, data = data)))
    }
    
    expr <- include_all_element_adders(expr)
    result_env <- rlang::new_environment(parent = env)
    result_env$.___ind_names <- get_loop_indices(expr)
    result_env$.___format_ind <- format_ind
    
    with(result_env, {
        .___result <- base::list()
        .___indices <- base::character()
        .___add_loop_element <- function(x) {
            ind <- mget(.___ind_names, envir = parent.frame(), ifnotfound = 0)
            ind <- ind[.___ind_names]
            
            val <- sapply(ind, .___format_ind)
            ind_txt <- paste0(names(ind), "=", val, collapse = ", ")
            
            x # prevents execution when there's a `next`
            
            .___indices <<- c(.___indices, ind_txt)
            .___result <<- c(.___result, list(x))
        }
    })
    
    rlang::eval_tidy(expr, data = data, env = result_env)
    result <- result_env$.___result
    names(result) <- result_env$.___indices
    
    result
}

# At the moment only checks that there is no `return` statement
check_for_split <- function(quosure, call = parent.frame()) {
    if ("return" %in% all.names(quosure)) {
        cli_abort(
            c("Cannot use `return` inside a for loop.",
              ">" = "Use `next` to skip the rest of the loop."),
            class = "lpsugar_error_return_in_for_loop",
            call = call
        )
    }
}

is_assignment <- function(expr) {
    rlang::is_call(expr) && 
        (expr[[1]] == quote(`<-`) || expr[[1]] == quote(`=`))
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

# Runs `include_element_adder()` on all nested for loops
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

# Replaces 
# `for (i in seq) {expr}`
# with
# `for (i in seq) .___add_loop_element({expr})`
include_element_adder <- function(expr) {
    last <- length(expr)
    e <- expr[[last]]
    
    # Does not add element if it's an assigment (<-)
    if (is_assignment(e)) {
        return(expr)
    }
    
    # This line keeps R CMD Check happy
    .___add_loop_element <- NULL
    
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
        here_index <- format(expr[[2]])
    } 
    else {
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
    } 
    else {
        rlang::as_label(i)
    }
}
