#' Comparing numerical values
#' 
#' Just a wrapper for comparing numerical values, for use with quicksort.
#' 
#' Just returns \code{-1} if \code{x} is less than \code{y}, \code{1} if
#' \code{x} is greater, and \code{0} if they are equal (according to
#' \code{==}).  The vectors wrap as usual if they are of different lengths.
#' 
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @return An integer vector.
#' @author Robin Evans
#' @seealso \code{`<`} for traditional Boolean operator.
#' @keywords arith
#' @examples
#' 
#' greaterThan(4,6)
#' 
#' # Use in sorting algorithm.
#' quickSort(c(5,2,9,7,6), f=greaterThan)
#' order(c(5,2,9,7,6))
#' 
#' @export greaterThan
greaterThan <-
function (x, y) 
{
    return(-1 * (x < y) + 1 * (x > y))
}
