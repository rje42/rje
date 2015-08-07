#' Matrix of Subset Indicators
#' 
#' Produces a matrix whose rows indicate what subsets of a set are included in
#' which other subsets.
#' 
#' This function returns a matrix, with each row and column corresponding to a
#' subset of a hypothetical set of size \code{n}, ordered lexographically.  The
#' entry in row \code{i}, column \code{j} corresponds to whether or not the
#' subset associated with \code{i} is a superset of that associated with
#' \code{j}.
#' 
#' A 1 or -1 indicates that \code{i} is a superset of \code{j}, with the sign
#' referring to the number of fewer elements in \code{j}.  0 indicates that
#' \code{i} is not a superset of \code{j}.
#' 
#' @param n integer containing the number of elements in the set.
#' @return An integer matrix of dimension 2^n by 2^n.
#' @note The inverse of the output matrix is just \code{abs(subsetMatrix(n))}.
#' @author Robin Evans
#' @seealso \code{\link{combinations}}, \code{\link{powerSet}}.
#' @keywords arith
#' @examples
#' 
#' subsetMatrix(3)
#' 
#' @export subsetMatrix
subsetMatrix <-
function (n) 
{
    out = matrix(1, 1, 1)
    for (i in seq_len(n)) {
        out = matrix(c(1, -1, 0, 1), 2, 2) %x% out
    }
    out
}


#' Orthogonal Design Matrix
#' 
#' Produces a matrix whose rows correspond to an orthogonal binary design matrix.
#' 
#' @param n integer containing the number of elements in the set.
#' @return An integer matrix of dimension 2^n by 2^n containing 1 and -1.
#' @note The output matrix has orthogonal columns and is symmetric, so (up to a constant) is its own inverse.
#' @author Robin Evans
#' @seealso \code{\link{combinations}}, \code{\link{subsetMatrix}}.
#' @keywords arith
#' @examples
#' 
#' designMatrix(3)
#' 
#' @export designMatrix
designMatrix <-
  function (n) 
  {
    out = matrix(1, 1, 1)
    for (i in seq_len(n)) {
      out = matrix(c(1, 1, 1, -1), 2, 2) %x% out
    }
    out
  }
