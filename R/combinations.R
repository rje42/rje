#' Combinations of Integers
#' 
#' Returns a matrix containing each possible combination of one entry from
#' vectors of the lengths provided.
#' 
#' Returns a matrix, each row being one possible combination of integers from
#' the vectors \eqn{(0, 1, \ldots, p_i-1)}{(0, 1,..., p_i - 1)}, for \eqn{i}{i}
#' between 1 and \code{length(p)}.
#' 
#' Based on \code{bincombinations} from package \code{e1071}, which provides
#' the binary case.
#' 
#' \code{powerSetMat} is just a wrapper for \code{combinations(rep(2, n))}.
#' 
#' @aliases combinations powerSetMat
#' @usage
#' combinations(p)
#' powerSetMat(n) 
#' 
#' @param p vector of non-negative integers.
#' @param n non-negative integer.
#' @return A matrix with number of columns equal to the length of \code{p}, and
#' number of rows equal to \eqn{p_1 \times \cdots \times p_k}{p_1 * ... * p_k},
#' each row corresponding to a different combination.  Ordering is
#' reverse-lexographic.
#' @author Robin Evans
#' @keywords arith
#' @examples
#' 
#' combinations(c(2,3,3))
#' 
#' powerSetMat(3)
#' 
#' @export combinations
combinations <-
function (p) 
{
    tot = prod(p)
    cp = cumprod(p)
    retval = rep.int(0, tot * length(p))
    dim(retval) = c(tot, length(p))
    for (i in seq_along(p)) {
        retval[, i] <- rep(seq_len(p[i]) - 1, each = cp[i]/p[i])
    }
    retval
}

#' @export powerSetMat
powerSetMat <- function (n) 
{
  combinations(rep.int(2, n))
}
