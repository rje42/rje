##' Row-wise minima and maxima
##'
##' @aliases rowMins rowMaxs
##' 
##' @param x a numeric (or logical) matrix or data frame
##'
##' @usage
##' rowMins(x)
##' rowMaxs(x)
##' 
##' @return numeric vector of length \code{nrow(x)} giving the row-wise minima (or maxima) of \code{x}.
##' 
##' @details The function coerces \code{x} to be a data frame and then
##' uses \code{pmin} (\code{pmax}) on it.  This is the same as
##' \code{apply(x, 1, min)} but generally faster if the number of rows
##' is large.
##' 
##' @export rowMins
rowMins = function(x) {
  do.call(pmin, as.data.frame(x))
}

##' @export rowMaxs
rowMaxs = function(x) {
  do.call(pmax, as.data.frame(x))
}
