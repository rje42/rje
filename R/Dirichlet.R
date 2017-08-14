##' The Dirichlet Distribution
##' 
##' @description Density function and random generation for Dirichlet distribution with %
##' parameter vector \code{alpha}.
##' 
##' @details If \code{x} is a matrix, each row is taken to be a different point whose %
##' density is to be evaluated.  If the number of columns in (or length of, in
##' the % case of a vector) \code{x} is one less than the length of
##' \code{alpha}, the % remaining column (or entry) is assumed to make the
##' vector sum to 1.
##' 
##' The \emph{k}-dimensional Dirichlet distribution has density
##' \deqn{\frac{\Gamma\left(\sum_i \alpha_i\right)}{\prod_i \Gamma(\alpha_i)}
##' \prod_{i=1}^k x_i^{\alpha_i-1}}{Gamma (alpha_1 + ... +
##' alpha_k)*(Gamma(alpha_1)*...*Gamma(alpha_k))^{-1}*x_1^{alpha_1-1}*...*x_k^{alpha_k-1}}
##' assuming that \eqn{x_i > 0}{x_i > 0} and \eqn{\sum_i x_i = 1}{x_1 + ... +
##' x_k = 1}, and zero otherwise.
##' 
##' If the sum of row entries in \code{x} differs from 1 by more than
##' \code{tol}, % or any entry takes a value less than \code{-tol}, the density
##' is assumed to be % zero.
##' 
##' @aliases Dirichlet ddirichlet rdirichlet
##' @usage 
##' ddirichlet(x, alpha, log = FALSE, tol = 1e-10) 
##' rdirichlet(n, alpha) 
##' 
##' @param n number of random variables to be generated.
##' @param alpha vector of Dirichlet hyper parameter.
##' @param x vector (or matrix) of points in sample space.
##' @param log logical; if TRUE, natural logarithm of density is returned.
##' @param tol tolerance of vectors not summing to 1 and negative values.
##' @return \code{rdirichlet} returns a matrix, each row of which is an
##' independent draw % from a Dirichlet distribution with parameter vector
##' \code{alpha}.
##' 
##' \code{ddirichlet} returns a vector, each entry being the density of the %
##' corresponding row of \code{x}.  If \code{x} is a vector, then the output %
##' will have length 1.
##' @author Robin Evans
##' @references \url{http://en.wikipedia.org/wiki/Dirichlet_distribution}
##' @keywords distribution
##' @examples
##' 
##' x = rdirichlet(10, c(1,2,3))
##' x
##' 
##' # Find densities at random points.
##' ddirichlet(x, c(1,2,3))
##' # Last column to be inferred.
##' ddirichlet(x[,c(1,2)], c(1,2,3))
##' 
##' @name Dirichlet
##' @importFrom stats rgamma
 
##' @export ddirichlet
ddirichlet <-
function(x, alpha, log = FALSE, tol=1e-10) {
  k = length(alpha)
  dimx = dim(x)
  if (is.matrix(x)) n = dimx[1]
  else if(is.numeric(x)) {
    dim(x) = dimx = c(1,length(x))
    n = 1
  }
  else stop("Vales must be numeric")
  if (any(alpha <= 0)) stop("Parameters must be positive")

  out = numeric(n)

  if (dimx[2] == k-1) {
    x = cbind(x,1-.rowSums(x, dimx[1], dimx[2]))
    rwsms = rep.int(1, n)
  }
  else if (dimx[2] != k) stop("Dimensions do not match")
  else rwsms = .rowSums(x, n, k)

  out[ rowMins(x) < -tol | abs(rwsms-1) > tol ] = 0

  out = colSums((alpha-1)*log(t(x))) + lgamma(sum(alpha)) - sum(lgamma(alpha))
  if (!log) out = exp(out)

  return(out)
}

##' @export rdirichlet
rdirichlet <- function (n, alpha) 
{
  if (is.matrix(alpha)) {
    k = dim(alpha)[2]
    alpha = as.vector(t(alpha))
  }
  else k = length(alpha)
  out = matrix(rgamma(n * k, shape = alpha), n, k, byrow = TRUE)
  out = out/.rowSums(out, n, k)

  out
}
