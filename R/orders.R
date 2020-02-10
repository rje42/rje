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
