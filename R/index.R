
# Legacy -------------------------------------------

# check out-of-bounds indices and makes drop = FALSE by default
robust_index <- function(x) {
    if (slam::is.simple_triplet_matrix(x)) {
        cli_warn("atempt to use robust_index() on a <simple_triplet_matrix>")
        return(x)
    }
    structure(x, class = c("robust_index", class(x)))
}

#' @export
print.robust_index <- function(x, ...) {
    print(unclass(x))
    cat("with class 'robust_index' from package 'lpsugar'\n")
    invisible(x)
}

#' @export
`[.robust_index` <- function(x, ..., drop = FALSE) {
    if (...length() == 1L) {
        check_index_valid_vector(x, index = c(...))
    } 
    else if (is.array(x)) {
        d <- dim(x)
        
        if (...length() != length(d)) {
            cli_abort("Incorrect number of dimensions.")
        }
        
        dots <- rlang::dots_list(..., .ignore_empty = "none", .preserve_empty = TRUE)
        
        for (margin in 1:length(d)) {
            dm <- dots[[margin]]
            
            if (rlang::is_missing(dm)) {
                next
            }
            
            im <- eval(dm, envir = parent.frame())
            
            check_index_valid_array(
                x,
                margin = margin,
                index = im
            )
        }
    } 
    else if (is.vector(x)) {
        if (...length() != 1L) {
            cli_abort("Incorrect number of dimensions.")
        }
        
        check_index_valid_vector(x, index = im)
    }
    
    NextMethod(drop = drop) |> robust_index()
}

check_index_valid_array <- function(x, margin, index, call = parent.frame()) {
    if (is.logical(index)) {
        if (anyNA(index)) {
            cli_abort("Subscript is NA", call = call)
        }
        if (length(index) != dim(x)[margin]) {
            cli_abort("Subscript length mismatch in dimension {margin}", call = call)
        }
    } 
    else if (is.numeric(index)) {
        zero <- match(TRUE, index > -1L & index < 1L)
        
        if (!is.na(zero)) {
            cli_abort("Invalid subscript ({index[zero]}) in dimension {margin}.", call = call)
        }
        
        cap <- dim(x)[margin]
        oob <- match(TRUE, index <= -cap - 1L | index >= cap + 1L)
        
        if (!is.na(oob)) {
            cli_abort("Subscript ({index[oob]}) out of bounds in dimension {margin}.", call = call)
        }
    } 
    else if (is.character(index) || is.factor(index)) {
        index <- as.character(index)
        nams <- dimnames(x)[[margin]]
        
        if (is.null(nams)) {
            cli_abort("Dimension {margin} is unnamed.", call = call)
        }
        
        missing_name <- match(TRUE, !is.element(index, nams))
        
        if (!is.na(missing_name)) {
            cli_abort("Invalid subscript '{index[missing_name]}' in dimension {margin}.", call = call)
        }
    } 
    else {
        cli_abort("Invalid subscript of class `{class(index)}`.", call = call)
    }
}
check_index_valid_vector <- function(x, index, call = parent.frame()) {
    if (is.logical(index)) {
        if (anyNA(index)) {
            cli_abort("Subscript is NA", call = call)
        }
        
        if (length(index) != length(x)) {
            cli_abort("Subscript length mismatch", call = call)
        }
    }
    else if (is.numeric(index)) {
        zero <- match(TRUE, index > -1L & index < 1L)
        
        if (!is.na(zero)) {
            cli_abort("Invalid subscript ({index[zero]}).", call = call)
        }
        
        cap <- length(x)
        oob <- match(TRUE, index <= -cap - 1L | index >= cap + 1L)
        
        if (!is.na(oob)) {
            cli_abort("Subscript ({index[oob]}) out of bounds.", call = call)
        }
    } 
    else if (is.character(index) || is.factor(index)) {
        index <- as.character(index)
        nams <- names(x)
        
        if (is.null(nams)) {
            cli_abort("Vector is unnamed.", call = call)
        }
        
        missing_name <- match(TRUE, !is.element(index, nams))
        
        if (!is.na(missing_name)) {
            cli_abort("Invalid subscript '{index[missing_name]}'.", call = call)
        }
    } 
    else {
        cli_abort("Invalid subscript of class `{class(index)}`.", call = call)
    }
}


# New --------------------------------------

# check out-of-bounds indices and makes drop = FALSE by default
strict_index <- function(x) {
    structure(x, class = c("strict_index", class(x)))
}

#' @export
print.strict_index <- function(x, ...) {
    print(unclass(x))
    cat("with class 'strict_index' from package 'lpsugar'\n")
    invisible(x)
}

#' @export
`[.strict_index` <- function(x, ..., drop = FALSE) {
    dots <- rlang::dots_list(..., .preserve_empty = TRUE, .ignore_empty = "none")
    args <- rlang::names2(dots)
    dn <- dimnames(x) %||% list(names(x))
    d <- dim2(x)
    
    if (length(dots) == 1L && ndim(x) != 1L) {
        check_index(i = dots[[1]], n = length(x), arg = args[[1]])
    }
    else if (length(dots) > 1L && length(dots) != ndim(x)) {
        cli_abort(
            c("Incorrect number of dimensions.",
              "x" = "Object has {ndim(x)} dimensions.")
        )
    }
    else for (k in seq_along(dots)) if (!rlang::is_missing(dots[[k]])) {
        check_index(i = dots[[k]], n = d[k], names = dn[[k]], arg = args[[k]])
    }
    
    NextMethod(drop = drop) |> strict_index()
}

check_index <- function(i, n, names = NULL, arg, call = parent.frame()) {
    if (is.factor(i)) {
        i <- as.character(i)
    }
    if (inherits(i, "strict_index")) {
        class(i) <- class(i) |> setdiff("strict_index")
    }
    if (is.numeric(i)) {
        vctrs::num_as_location(
            i, n, 
            missing = "error", 
            negative = "invert", 
            oob = "error",
            zero = "error",
            arg = arg,
            call = call
        )
    }
    else {
        vctrs::vec_as_location(
            i, n,
            names = names,
            missing = "error",
            arg = arg,
            call = call
        )
    }
}
