#' Faster calculation of array indices from vector position.
#' 
#' Calculates array indices based on their vector position in an array.
#' 
#' This is a \code{C} implementation of the \code{base} function of the same
#' name.  Results should be the same.
#' 
#' Given a vector of integers giving the vector position of entries in an
#' array, returns the appropriate array indices.
#' 
#' @param ind integer-valued vector of indices.
#' @param .dim integer vector givine dimensions of array.
#' @param .dimnames optional list of character \code{\link{dimnames}(.)}, of
#' which only \code{.dimnames[[1]]} is used.
#' @param useNames logical indicating if the value of \code{arrayInd()} should
#' have (non-null) dimnames at all.
#' @return A matrix whose rows each are the indices of one element of \code{x};
#' see Examples below.
#' @author Robin Evans
#' @seealso The base version, documented as \code{\link[base]{which}}.
#' @keywords array
#' @examples
#' 
#' arr = array(1:36, dim=c(2,3,2,3))
#' ind = arrayInd(c(4,9,17), c(2,3,2,3))
#' ind
#' 
#' arr[2,2,1,1]
#' arr[1,2,2,1]
#' arr[1,3,1,2]
#' 
#' @export arrayInd
arrayInd <-
function (ind, .dim, .dimnames = NULL, useNames = FALSE) 
{
    rank = length(.dim)
    call = if (length(ind)) {
        .C("arrayInd", as.integer(ind - 1), as.integer(.dim), 
            length(ind), rank, integer(rank * length(ind)), package = "rje")[[5]]
    }
    else integer(0)
    out = matrix(call, nrow = length(ind), ncol = rank)
    if (useNames) 
        dimnames(out) = list(.dimnames[[1L]][ind], if (rank == 
            2L) c("row", "col") else paste0("dim", seq_len(rank)))
    out
}
