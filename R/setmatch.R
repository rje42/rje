#' Set Operations
#' 
#' Series of functions extending existing vector operations to lists of
#' vectors.
#' 
#' \code{setmatch} checks whether each vector in the list \code{x} is also
#' contained in the list \code{y}, and if so returns position of the first such
#' vector in \code{y}.  The ordering of the elements of the vector is
#' irrelevant, as they are considered to be sets.
#' 
#' \code{subsetmatch} is similar to \code{setmatch}, except vectors in \code{x}
#' are searched to see if they are subsets of vectors in \code{y}.
#' 
#' \code{setsetdiff} is a setwise version of \code{setdiff}, and
#' \code{setsetequal} a setwise version of \code{setequal}.
#' 
#' @aliases setmatch subsetmatch setsetdiff setsetequal
#' @param x list of vectors.
#' @param y list of vectors.
#' @param nomatch value to be returned in the case when no match is found. Note
#' that it is coerced to integer.
#' @return \code{setmatch} and \code{subsetmatch} return a vector of integers
#' of length the same as the list \code{x}.
#' 
#' \code{setsetdiff} returns a sublist \code{x}.
#' 
#' \code{setsetequal} returns a logical of length 1.
#' @note These functions are not recursive, in the sense that they cannot be
#' used to test lists of lists.  They also do not reduce to the vector case.
#' @author Robin Evans
#' @seealso \code{\link{match}}, \code{\link{setequal}}, \code{\link{setdiff}}
#' @keywords arith
#' @examples
#' 
#' x = list(1:2, 1:3)
#' y = list(1:4, 1:3)
#' setmatch(x, y)
#' subsetmatch(x, y)
#' setsetdiff(x, y)
#' 
#' x = list(1:3, 1:2)
#' y = list(2:1, c(2,1,3))
#' setsetequal(x, y)
#' 
#' @export setmatch
setmatch <-
function (x, y, nomatch = NA_integer_) 
{
    if (!is.list(x) && !is.list(y)) 
        stop("Arguments must be lists")
    out = rep.int(nomatch, length(x))
    for (i in seq_along(x)) {
        for (j in seq_along(y)) {
            if ((x[[i]] %subof% y[[j]]) && (y[[j]] %subof% x[[i]])) {
                out[i] = j
                break
            }
        }
    }
    out
}
