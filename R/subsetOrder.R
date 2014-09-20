#' Compare sets for inclusion.
#' 
#' A wrapper for \code{is.subset} which returns set inclusions.
#' 
#' If \code{x} is a subset of \code{y}, returns \code{-1}, for the reverse
#' returns \code{1}.  If sets are equal or incomparable, it returns \code{0}.
#' 
#' @param x A vector.
#' @param y A vector of the same type as \code{x}.
#' @return A single integer, 0, -1 or 1.
#' @author Robin Evans
#' @seealso \code{\link{is.subset}}.
#' @keywords arith
#' @examples
#' 
#' subsetOrder(2:4, 1:4)
#' subsetOrder(2:4, 3:5)
#' 
#' @export subsetOrder
subsetOrder <-
function (x, y) 
{
    if (setequal(x, y)) 
        return(0)
    else if (is.subset(x, y)) 
        return(-1)
    else if (is.subset(y, x)) 
        return(1)
    else return(0)
}
