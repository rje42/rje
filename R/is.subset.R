#' Check subset inclusion
#' 
#' Determines whether one vector contains all the elements of another.
#' 
#' Determines whether or not every element of \code{x} is also found in
#' \code{y}. Returns \code{TRUE} if so, and \code{FALSE} if not.
#' 
#' @aliases is.subset %subof%
#' @param x vector.
#' @param y vector.
#' @return A logical of length 1.
#' @author Robin Evans
#' @seealso \code{\link{setmatch}}.
#' @keywords arith
#' @examples
#' 
#' is.subset(1:2, 1:3)
#' is.subset(1:2, 2:3)
#' 
#' @export is.subset
is.subset <-
function (x, y) 
{
    return(all(x %in% y))
}
