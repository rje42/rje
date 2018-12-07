#' Get indices of adjacent entries in array
#' 
#' Determines the relative vector positions of entries which are adjacent in an
#' array.
#' 
#' Given a particular cell in an array, which are the entries within (for
#' example) 1 unit in any direction?  This function gives the (relative) value
#' of such indices.  See examples.
#' 
#' Indices may be repeated if the range exceeds the size of the array in any
#' dimension.
#' 
#' @param upp A vector of non-negative integers, giving the distance in the
#' positive direction from the centre in each co-ordinate.
#' @param lwr A vector of non-positive integers, giving the negative distance
#' from the centre.
#' @param dim integer vector of array dimensions.
#' @return An integer vector giving relative positions of the indices.
#' @author Robin Evans
#' @seealso \code{\link[base]{arrayInd}}.
#' @keywords array
#' @examples
#' 
#' arr = array(1:144, dim=c(3,4,3,4))
#' arr[2,2,2,3]
#' # which are entries within 1 unit each each direction of 2,2,2,3?
#' 
#' inds = 89 + indexBox(1,-1,c(3,4,3,4))
#' inds = inds[inds > 0 & inds <= 144]
#' arrayInd(inds, c(3,4,3,4))
#' 
#' # what about just in second dimension?
#' inds = 89 + indexBox(c(0,1,0,0),c(0,-1,0,0),c(3,4,3,4))
#' inds = inds[inds > 0 & inds <= 144]
#' arrayInd(inds, c(3,4,3,4))
#' 
#' @export indexBox
indexBox <-
function (upp, lwr, dim) 
{
    if (any(upp < 0) || any(lwr > 0)) 
        stop("Incorrect bounds")
    if (any(dim <= 0)) 
        stop("Incorrect dimensions")
    ld = length(dim)
    if (length(upp) != ld) 
        upp = rep(upp, length.out = ld)
    if (length(lwr) != ld) 
        lwr = rep(lwr, length.out = ld)
    out = .C("indexBox_c", as.integer(upp), as.integer(lwr), as.integer(dim), 
        ld, integer(prod(upp - lwr + 1)), PACKAGE = "rje")
    return(out[[5]])
}
