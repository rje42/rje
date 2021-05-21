##' Get inclusion maximal subsets from a list
##' 
##' @param x list containing the subsets
##' @param right logical indicating 
##' 
##' @details Returns the inclusion maximal elements of \code{x}.  The 
##' indicator \code{right} may be set to \code{TRUE} in order to indicate
##' that the right-most entry is always an inclusion maximal set.
##' 
##' @examples 
##' letlist <- list(LETTERS[1:2], LETTERS[2:4], LETTERS[1:3])
##' inclusionMax(letlist)
##' 
##' @export
inclusionMax <- function (x, right=FALSE) {
  if (length(x) <= 1) return(x)
  
  ## if not provided sorted, then do that first
  if (!right) {
    lets <- unique.default(unlist(x))
    if (length(lets) > 3000) stop("this function only allows at most 3000 distinct objects")
    x2 <- sapply(x, function(y) sum(1.01^(match(y, lets)-1)))
    if (any(is.na(x2))) stop("Error in matching items")
    x <- x[order(x2)]
  }
  
  out <- list()
  
  while (length(x) > 0) {
    set <- x[length(x)]
    out <- c(set, out)  
    rm <- subsetmatch(x, set, nomatch=0) > 0
    x <- x[!rm]
  }
  
  out
}
