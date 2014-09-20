#' Fast pairwise logical operators
#' 
#' Fast but loose implementations of AND and OR logical operators.
#' 
#' Returns pairwise application of logical operators AND and OR.  Vectors are
#' recycled as usual.
#' 
#' @aliases and0 or0
#' @param x,y logical or numerical vectors
#' @return A logical vector of length \code{max(length(x), length(y))} with
#' entries \code{x[1] & x[2]} etc.; each entry of \code{x} or \code{y} is
#' \code{TRUE} if it is non-zero.
#' @note These functions should only be used with well understood vectors, and
#' may not deal with unusual cases correctly.
#' @examples
#' 
#' and0(c(0,1,0), c(1,1,0))
#' \dontrun{
#' set.seed(1234)
#' x = rbinom(5000, 1, 0.5)
#' y = rbinom(5000, 1, 0.5)
#' 
#' # 3 to 4 times improvement over `&`
#' system.time(for (i in 1:5000) and0(x,y))
#' system.time(for (i in 1:5000) x & y)
#' }
#' 
#' @export and0
and0 <-
function (x, y) 
{
    as.logical(x * y)
}

#' @export or0
or0 <-
  function (x, y) 
  {
    as.logical(x + y)
  }
