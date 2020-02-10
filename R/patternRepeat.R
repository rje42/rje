#' Complex repetitions
#' 
#' Recreate patterns for collapsed arrays
#' 
#' 
#' These functions allow for the construction of complex repeating patterns
#' corresponding to those obtained by unwrapping arrays.  Consider an array
#' with dimensions \code{n}; then for each value of the dimensions in
#' \code{which}, this function returns a vector which places the corresponding
#' entry of \code{x} into every place which would match this pattern when the
#' full array is unwrapped.
#' 
#' For example, if a full 4-way array has dimensions 2*2*2*2 and we consider
#' the margin of variables 2 and 4, then the function returns the pattern
#' c(1,1,2,2,1,1,2,2,3,3,4,4,3,3,4,4).  The entries 1,2,3,4 correspond to the
#' patterns (0,0), (1,0), (0,1) and (1,1) for the 2nd and 4th indices.
#' 
#' In \code{patternRepeat()} the argument \code{x} is repeated according to the
#' pattern, while \code{patternRepeat0()} just returns the indexing pattern.
#' So \code{patternRepeat(x,which,n)} is effectively equivalent to
#' \code{x[patternRepeat0(which,n)]}.
#' 
#' The length of \code{x} must be equal to \code{prod(n[which])}.
#' 
#' @aliases patternRepeat patternRepeat0
#' @param x A vector to be repeated.
#' @param which Which indices of the implicit array are given in \code{x}.
#' @param n Dimensions of implicit array.
#' @param careful logical indicating whether to check vailidty of arguments,
#' but therefore slow things down.
#' @param keep.order logical indicating whether to respect the ordering of the
#' entries in the vector \code{which}, in which case data are permuted before
#' replication.  In other words, does \code{x} change fastest in
#' \code{which[1]}, or in the minimal entry for \code{which}?
#' @return Both return a vector of length \code{prod(n)};
#' \code{patternRepeat()} one containing suitably repeated and ordered elements
#' of \code{x}, for \code{patternRepeat0()} it is always the integers from 1 up
#' to \code{prod(n[which])}.
#' @author Robin Evans
#' @seealso \code{\link{rep}}
#' @keywords array
#' @examples
#' 
#' patternRepeat(1:4, c(1,2), c(2,2,2))
#' c(array(1:4, c(2,2,2)))
#' 
#' patternRepeat0(c(1,3), c(2,2,2))
#' patternRepeat0(c(2,3), c(2,2,2))
#' 
#' patternRepeat0(c(3,1), c(2,2,2))
#' patternRepeat0(c(3,1), c(2,2,2), keep.order=TRUE)
#' 
#' patternRepeat(letters[1:4], c(1,3), c(2,2,2))
#' 
#' @export patternRepeat
patternRepeat <-
function (x, which, n, careful = TRUE, keep.order = FALSE) 
{
    if (careful) {
        if (length(which) == 0) {
            if (length(x) != 1) 
                stop("x not of correct length")
            else return(rep.int(x, prod(n)))
        }
        if (prod(n[which]) != length(x)) 
            stop("x not of correct length")
    }
    else if (length(which) == 0) 
        return(rep.int(x, prod(n)))
    idx = patternRepeat0(which, n, careful, keep.order)
    return(x[idx])
}

#' @export patternRepeat0
patternRepeat0 <-
  function (which, n, careful = TRUE, keep.order = FALSE) 
  {
    out_size = as.integer(prod(n))
    if (length(which) == 0) 
      return(rep.int(1, out_size))
    out = integer(out_size)
    if (out_size == 0) 
      return(out)
    else if (out_size < 0) 
      stop("Dimensions must be positive")
    which = as.integer(which - 1L)
    if (!keep.order) 
      out[1] = 1L
    if (careful) {
      if (any(which >= length(n))) 
        stop("Indices in 'which' do not match length of dimensions in 'n'")
      if (any(which < 0)) 
        stop("Indices in 'which' must be positive integers")
      if (anyDuplicated(which)) {
        which = unique.default(which)
        warning("Duplicated indices ignored")
      }
    }
    out = .C("patternRepeat_c", which, length(which), as.integer(n), 
             length(n), out)[[5]]
    out
  }
