##' Get inclusion maximal subsets from a list
##' 
##' @param x list containing the subsets
##' @param right logical indicating whether right-most entry is always inclusion maximal
##' 
##' @details Returns the inclusion maximal elements of \code{x}.  The 
##' indicator \code{right} may be set to \code{TRUE} in order to indicate
##' that the right-most entry is always an inclusion maximal set over all earlier 
##' sets.
##' 
##' @examples 
##' letlist <- list(LETTERS[1:2], LETTERS[2:4], LETTERS[1:3])
##' inclusionMax(letlist)
##' 
##' @export
inclusionMax <- function (x, right=FALSE) {
  if (length(x) <= 1) return(x)
  
  ## reorder sets so that we start with largest
  if (!right) {
    lens <- lengths(x)
    x <- x[order(lens)]
  }
  
  ## list for inclusion maximal entries
  out <- list()
  
  while (length(x) > 0) {
    set <- last(x)
    # set <- x[length(x)]
    out <- c(set, out)  
    rm <- subsetmatch(x, set, nomatch=0) > 0
    x <- x[!rm]
  }
  
  out
}
