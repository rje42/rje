#' Set Operations
#' 
#' Series of functions extending existing vector operations to lists of
#' vectors.
#' 
#' `setmatch` checks whether each vector in the list `x` is also
#' contained in the list `y`, and if so returns position of the first such
#' vector in `y`.  The ordering of the elements of the vector is
#' irrelevant, as they are considered to be sets.
#' 
#' `subsetmatch` is similar to `setmatch`, except vectors in `x`
#' are searched to see if they are subsets of vectors in `y`.  Similarly
#' `supersetmatch` consideres if vectors in `x` are supersets of
#' vectors in `y`.
#' 
#' `setsetdiff` is a setwise version of `setdiff`, and
#' `setsetequal` a setwise version of `setequal`.
#' 
#' @aliases setmatch subsetmatch setsetdiff setsetequal
#' @param x list of vectors.
#' @param y list of vectors.
#' @param nomatch value to be returned in the case when no match is found. Note
#' that it is coerced to integer.
#' @return `setmatch` and `subsetmatch` return a vector of integers
#' of length the same as the list `x`.
#' 
#' `setsetdiff` returns a sublist `x`.
#' 
#' `setsetequal` returns a logical of length 1.
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
    if (!is.list(x) || !is.list(y)) 
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

#' @describeIn setmatch Test for equality of sets
#' @export setsetequal
setsetequal <-
    function (x, y) 
    {
        if (!is.list(x) && !is.list(y)) 
            stop("Arguments must be lists")
        all(c(setmatch(x, y, 0L) > 0L, setmatch(y, x, 0L) > 0L))
    }

#' @describeIn setmatch Setdiff for lists
#' @export setsetdiff
setsetdiff <-
    function (x, y) 
    {
        if (!is.list(x) && !is.list(y)) 
            stop("Arguments must be lists")
        x[match(x, y, 0L) == 0L]
    }

#' @describeIn setmatch Test for subsets
#' @export subsetmatch
subsetmatch <-
    function (x, y, nomatch = NA_integer_) 
    {
        if (!is.list(x) || !is.list(y)) 
            stop("Arguments must be lists")
        out = rep.int(nomatch, length(x))
        for (i in seq_along(x)) {
            for (j in seq_along(y)) {
                if (x[[i]] %subof% y[[j]]) {
                    out[i] = j
                    break
                }
            }
        }
        out
    }

#' @describeIn setmatch Test for supersets
#' @export 
supersetmatch <-
    function (x, y, nomatch = NA_integer_) 
    {
        if (!is.list(x) || !is.list(y)) 
            stop("Arguments must be lists")
        out = rep.int(nomatch, length(x))
        for (i in seq_along(x)) {
            for (j in seq_along(y)) {
                if (y[[j]] %subof% x[[i]]) {
                    out[i] = j
                    break
                }
            }
        }
        out
    }

##' Check list of sets is nested
##' 
##' @param x list containing collection of sets
##' 
##' @return If the sets are nested it returns an ordering, otherwise `NA`.
##' 
##' @export
sets_nested <- function (x) {
  ord <- order(lengths(x))

  ## can probably be sped up by ordering sets before comparison
  for (i in seq_along(ord)[-1]) {
    ## if adjacent sets are not nested, then function fails
    if (!all(x[[ord[i-1]]] %in% x[[ord[i]]])) return(NA)
  }
  
  return(ord)
}

