##' Generate a joint (or conditional) probability distribution
##' 
##' @description Wrapper functions to quickly generate discrete joint
##' (or conditional) distributions using Dirichlets
##' 
##' @param dim the joint dimension of the probability table
##' @param d number of dimensions
##' @param cond optionally, vertices to condition upon
##' @param alpha Dirichlet hyper parameter, defaults to 1 (flat density).
##'
##' @details \code{rprobdist} gives an array of dimension \code{dim}
##' (recycled as necessary to have length \code{d}, if this is
##' supplied) whose entries are probabilities drawn from a Dirichlet
##' distribution whose parameter vector has entries equal to
##' \code{alpha} (appropriately recycled).
##'
##' @return an array of appropriate dimensions
##' 
##' @author Robin Evans
##' @keywords distribution
##' @examples
##' rprobdist(2, 4)     # 2x2x2x2 table
##' rprobdist(c(2,3,2)) # 2x3x2 table
##'
##' rprobdist(2, 4, alpha=1/16)     # using unit information prior
##' 
##' rprobdist(2, 4, cond=c(2,4), alpha=1/16) # get variables 2 and 4
##' conditioned upon
##'
##' @section Side Effects:
##' Uses as many gamma random variables as cells
##' in the table, so will alter the random seed accordingly.
##' 
##' @export rprobdist
rprobdist <- function (dim, d, cond, alpha=1) 
{
  ## if dimension vector shorter than length d, recycle (with warning
  ## if necessary)
  if (missing(d)) d = length(dim)
  else if (length(dim) < d) dim = dim * rep.int(1L, d)
  else if (length(dim) > d) stop("More than 'd' dimensions supplied")
  
  if (any(dim < 0)) stop("Dimensions must be non-negative")
  if (any(alpha < 0)) stop("Parameters must be non-negative")

  pars = rep(alpha, length.out=prod(dim))
  
  out = c(rdirichlet(1, pars))
  if (length(dim) > 1) dim(out) = dim
  if (!missing(cond)) {
    if (any(cond > length(dim)) || any(cond <= 0)) stop("cond must be ")
    out = conditionTable(out, seq_along(dim)[-cond], cond, order=FALSE)
  }

  return(out)
}

