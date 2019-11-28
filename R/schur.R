##' Obtain generalized Schur complement
##'
##' @param M symmetric positive definite matrix
##' @param x,y,z indices of M to calculate with (see below)
##'
##' @details Calculates \eqn{M_{xy} - M_{xz} M^{zz} M_{zy}}, which
##' (if M is a Gaussian covariance matrix) is the covariance between
##' x and y after conditioning on z.
##' 
##' y defaults to equal x, and z to be the complement of \eqn{x \cup y}.
##' 
##' @export
schur = function(M,x,y,z) {
  if(!is.matrix(M) || nrow(M) != ncol(M)) stop("M must be a square matrix")
  if (missing(y)) y = x
  if (missing(z)) z = seq_len(nrow(M))[-c(x,y)]
  
  if (length(z) == 0) return(M[x,y,drop=FALSE])
  
  Mi <- solve.default(M[z,z,drop=FALSE])
  
  if (length(x) < length(y)) {
    M[x,y,drop=FALSE] - (M[x,z,drop=FALSE] %*% Mi) %*% M[z,y,drop=FALSE]
  }
  else M[x,y,drop=FALSE] - M[x,z,drop=FALSE] %*% (Mi %*% M[z,y,drop=FALSE])
}
