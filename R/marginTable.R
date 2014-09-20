#' Compute margin of a table faster
#' 
#' @description Computes the margin of a contingency table given as an array, by
#'   summing out over the dimensions not specified.
#'   
#' @param x a numeric array
#' @param margin integer vector giving margin to be calculated (1 for rows, 
#'   etc.)
#' @param order logical - should indices of output be ordered as in the vector 
#'   \code{margin}?  Defaults to \code{TRUE}.
#'   
#' @aliases propTable
#'   
#' @details   With \code{order = TRUE} this is the same as the base function 
#'   \code{margin.table()}, but faster.
#'   
#'   With \code{order = FALSE} the function is even faster, but the indices in 
#'   the margin are returned in their original order, regardless of the way they
#'   are specified in \code{margin}.
#'   
#'   \code{propTable()} returns a renormalized contingency table whose entries 
#'   sum to 1. It is equivalent to \code{prop.table()}, but faster.
#'   
#' @return The relevant marginal table.  The class of \code{x} is copied to the 
#'   output table, except in the summation case.
#'   
#' @note Original functions are \code{\link[base]{margin.table}} and 
#'   \code{\link[base]{prop.table}}.
#' 
#' @concept margin
#' @keywords array
#'       
#' @examples
#' m <- matrix(1:4, 2)
#' marginTable(m, 1)
#' marginTable(m, 2)
#' 
#' propTable(m, 2)
#' 
#' # 3-way example
#' m <- array(1:8, rep(2,3))
#' marginTable(m, c(2,3))
#' marginTable(m, c(3,2))
#' marginTable(m, c(3,2), order=FALSE)
marginTable <-
function (x, margin = NULL, order = TRUE) 
{
    k = length(dim(x))
    if (length(margin) > 0) 
        rmv = seq_len(k)[-margin]
    else return(sum(x))
    dx = dim(x)
    if (!is.double(x)) 
        x = as.double(x)
    out2 = .C("marginTable", x, as.integer(dx), as.integer(k), 
        as.integer(rmv), as.integer(length(rmv)), package = "rje")[[1]]
    if (order) {
        sm = sort.int(margin)
        kpm = dx[sm]
        out = out2[seq_len(prod(kpm))]
        dim(out) = kpm
        if (any(sm != margin)) 
            out = aperm.default(out, rank(margin))
        dimnames(out) = dimnames(x)[sm]
    }
    else {
        kpm = dx[margin]
        out = out2[seq_len(prod(kpm))]
        dim(out) = kpm
        dimnames(out) = dimnames(x)[margin]
    }
    out
}
