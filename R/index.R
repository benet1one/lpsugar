# New Objects ------------------------------

as_nodrop <- function(x) {
    if (inherits(x, "lpsugar_nodrop")) {
        x
    } else {
        structure(x, class = c("lpsugar_nodrop", class(x)))
    }
}

unclass_nodrop <- function(x) {
    class(x) <- class(x)[class(x) != "lpsugar_nodrop"]
    x
}

#' @export
print.lpsugar_nodrop <- function(x, ...) {
    print(unclass(x))
    cat("with class 'lpsugar_nodrop'\n")
    invisible(x)
}

#' @export
`[.lpsugar_nodrop` <- function(x, ..., drop = FALSE) {
    NextMethod(drop = drop) |> as_nodrop()
}

# Legacy Objects ---------------------------

# check out-of-bounds indices and makes drop = FALSE by default
as_robust_index <- function(x) {
    if (slam::is.simple_triplet_matrix(x)) {
        cli_warn("atempt to use robust_index() on a <simple_triplet_matrix>")
        return(x)
    }
    if (inherits(x, "lpsugar_robust_index")) {
        x
    } else {
        structure(x, class = c("lpsugar_robust_index", class(x)))
    }
}

#' @export
print.lpsugar_robust_index <- function(x, ...) {
    print(unclass(x))
    cat("with class 'lpsugar_robust_index'\n")
    invisible(x)
}

#' @export
`[.lpsugar_robust_index` <- function(x, ..., drop = FALSE) {
    if (...length() == 1L) {
        check_index_valid(x, ind = ...elt(1L), margin = 0L)
    }
    else if (is.array(x)) {
        nsub <- ...length()
        nd <- ndim(x)
        
        if (nsub != nd) {
            cli_abort(
                c("Incorrect number of dimensions.",
                  "x" = "Specified {nsub} subscripts.",
                  "x" = "Object has {nd} dimension{?s}."),
                class = "lpsugar_error_invalid_index"
            )
        }
        
        dots <- rlang::dots_list(..., .ignore_empty = "none", .preserve_empty = TRUE)
        
        for (margin in 1:nd) {
            im <- dots[[margin]]
            
            if (rlang::is_missing(im)) {
                next
            }
            
            check_index_valid(x, ind = im, margin = margin)
        }
    } 
    
    NextMethod(drop = drop) |> as_robust_index()
}

check_index_valid <- function(x, ind, margin = 1L, call = parent.frame()) {
    if (margin == 0L) {
        n <- length(x)
        nams <- names(x)
        n_txt <- paste0("Object is length ", n, ".")
        in_dim <- ""
    }
    else {
        n <- dim2(x)[margin]
        nams <- dimnames(x)[[margin]]
        dnn <- names(dimnames(x))[margin]
        dn_txt <- if (length(dnn) == 1L && dnn != "") {
            paste0(margin, " (", dnn, ")")
        } else {
            margin
        }
        
        n_txt <- paste0("Dimension ", dn_txt, " is length ", n, ".")
        in_dim <- paste(" in dimension", dn_txt)
    }
    
    if (anyNA(ind)) {
        cli_abort(
            "Subscript contains NA values{in_dim}.",
            class = "lpsugar_error_invalid_index",
            call = call
        )
    }
    
    if (is.logical(ind)) {
        if (length(ind) != n) {
            cli_abort(
                c("Subscript length mismatch{in_dim}.",
                  "x" = "Logical subscript is length {length(ind)}.",
                  "x" = n_txt),
                class = "lpsugar_error_invalid_index",
                call = call
            )
        }
    }
    else if (is.numeric(ind)) {
        zero <- match(TRUE, ind > -1L & ind < 1L)
        
        if (!is.na(zero)) {
            cli_abort(
                "Invalid subscript ({ind[zero]}){in_dim}.", 
                class = "lpsugar_error_invalid_index",
                call = call
            )
        }

        oob <- match(TRUE, ind <= -n - 1L | ind >= n + 1L)
        
        if (!is.na(oob)) {
            cli_abort(
                c("Subscript out of bounds{in_dim}.", 
                  "x" = "Invalid subscript: {ind[oob]}.",
                  "x" = n_txt),
                class = "lpsugar_error_invalid_index",
                call = call
            )
        }
    }
    else if (is.character(ind) || is.factor(ind)) {
        if (is.null(nams)) {
            cli_abort(
                c("Object is unnamed{in_dim}.",
                  "x" = "Subscript is class <{class(ind)[1]}>."), 
                class = "lpsugar_error_invalid_index",
                call = call
            )
        }
        
        ind <- as.character(ind)
        missing_name <- match(FALSE, ind %in% nams)
        
        if (!is.na(missing_name)) {
            cli_abort(
                c("Invalid subscript {in_dim}.",
                  "x" = "Invalid subscript: {ind[missing_name]}.",
                  "x" = "Names{in_dim} are: {nams}."), 
                class = "lpsugar_error_invalid_index",
                call = call
            )
        }
    } 
    else {
        cli_abort(
            "Invalid subscript of class <{class(ind)[1]}>.", 
            class = "lpsugar_error_invalid_index",
            call = call
        )
    }
}
