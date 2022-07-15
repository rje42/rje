##' Alternate between sets and integers representing sets of integers via bits
##' 
##' @param n integer respresenting a set
##' @param index integer to start from
##' @param simplify logical: return a single list if \code{n} has length 1?
##' 
##' @details Converts an integer into its binary representation and 
##' interprets this as a set of integers.  Cannot handle sets with more
##' than 31 elements.
##' 
##' @return For \code{int2set} a list of sets one for each integer 
##' supplied, for \code{set2int} a vector of the same length as the number 
##' of sets supplied.
##' 
##' @export
int2set <- function (n, index=1, simplify=FALSE) {
  bn <- matrix(intToBits(n), nrow=32)
  out <- apply(bn, 2, function(x) which(x > 0), simplify = FALSE)
  if (index != 1) out <- lapply(out, function(x) x+index-1)
  
  if (simplify && length(out) == 1) return(out[[1]]) 
  return(out)
}

##' @describeIn int2set Convert sets to integers
##' @param x list of sets
##' @export
set2int <- function (x, index=1) {
  sapply(x, function(y) sum(2^(y-index)))
}

