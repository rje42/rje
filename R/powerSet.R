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
#' 
#' @details \code{powerSet} returns subsets up to size \code{m} (if this is specified).  
#' \code{powerSetCond} includes some non-empty subset of \code{y} in every set.
#' 
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
#' powerSetCond(2:3, y=1)
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

##' @param y set to condition on
##' @param sort logical: should sets be sorted?
##' @describeIn powerSet Add sets that can't be empty
##' @export
powerSetCond <-
function (x, y, m, rev = FALSE, sort=FALSE)
{
  if (missing(y) || length(y) == 0) return(powerSet(x, m, rev))

  if (missing(x)) x <- integer(0)
  
  x <- setdiff(x,y)
  
  out <- list()

  out_y <- powerSet(y, m, rev)[-1]
  out_x <- powerSet(x, m, rev)

  for (i in seq_along(out_y)) {
    for (j in seq_along(out_x)) {
      out[[j+length(out_x)*(i-1)]] <- c(out_y[[i]], out_x[[j]])
    }
  }

  ## remove large sets and optionally sort entries  
  if (!missing(m)) out <- out[lengths(out) <= m]
  if (sort) out <- lapply(out, sort.int)

  return(out)
}