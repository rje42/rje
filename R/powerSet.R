#' Power Set
#' 
#' Produces the power set of a vector.
#' 
#' Creates a list containing every subset of the elements of the vector
#' \code{x}.
#' 
#' @param x vector of elements (the set).
#' @param m maximum cardinality of subsets
#' @param rev logical indicating whether to reverse the order of subsets.
#' @return A list of vectors of the same type as \code{x}.
#' 
#' With \code{rev = FALSE} (the defualt) the list is ordered such that all
#' subsets containing the last element of \code{x} come after those which do
#' not, and so on.
#' @author Robin Evans
#' @seealso \code{\link{powerSetMat}}.
#' @keywords arith
#' @examples
#' 
#' powerSet(1:3)
#' powerSet(letters[3:5], rev=TRUE)
#' powerSet(1:5, m=2)
#' 
#' @export powerSet
powerSet <-
function (x, m, rev = FALSE) 
{
    if (missing(m)) m = length(x)
    if (m == 0) return(list(x[c()]))
  
    out = list(x[c()])
    if (length(x) == 1) 
        return(c(out, list(x)))
    for (i in seq_along(x)) {
        if (rev) 
            out = c(lapply(out[lengths(out) < m], function(y) c(y, x[i])), out)
        else out = c(out, lapply(out[lengths(out) < m], function(y) c(y, x[i])))
    }
    out
}
