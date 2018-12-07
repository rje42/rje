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
#' @seealso \code{\link{combinations}}, \code{\link{powerSet}},  \code{\link{designMatrix}}.
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
    M = matrix(c(1, -1, 0, 1), 2, 2)
    for (i in seq_len(n)) {
        out = .kronecker(M, out, make.dimnames = FALSE)
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
#' Operations with this matrix can be performed more efficiently using the fast Hadamard transform. 
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
    M = matrix(c(1, 1, 1, -1), 2, 2)
    for (i in seq_len(n)) {
      out = .kronecker(M, out, make.dimnames=FALSE)
    }
    out
  }

#' Compute fast Hadamard-transform of vector
#' 
#' Passes vector through Hadamard orthogonal design matrix.  Also known
#' as the Fast Walsh-Hadamard transform.
#' 
#' @param x vector of values to be transformed
#' @param pad optional logical asking whether vector not of length \eqn{2^k} should be
#' padded with zeroes
#' @details This is equivalent to multiplying by \code{designMatrix(log2(length(x)))} 
#' but should run much faster
#' @return A vector of the same length as x
#' @author Robin Evans
#' @seealso \code{\link{designMatrix}}, \code{\link{subsetMatrix}}.
#' @keywords arith
#' @examples
#' 
#' fastHadamard(1:8)
#' fastHadamard(1:5, pad=TRUE)
#' 
#' @export fastHadamard
fastHadamard <- function(x, pad=FALSE) {
  len <- length(x)
  k <- ceiling(log2(len))
  if (k != log2(len)) {
    if (!pad) stop("If 'pad=FALSE' then length must be a power of 2")
    else {
      x <- c(x, rep(0,2^k - len))
    }
  }
  
  out <- .C("hadamard_c", as.double(x), as.integer(k), PACKAGE = "rje")[[1]]
  
  out
}

