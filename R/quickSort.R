#' Quicksort for Partial Orderings
#' 
#' Implements the quicksort algorithm for partial orderings based on pairwise
#' comparisons.
#' 
#' Implements the usual quicksort algorithm, but may return the same positions
#' for items which are incomparable (or equal).  Does not test the validity of
#' \code{f} as a partial order.
#' 
#' If \code{x} is a numeric vector with distinct entries, this behaves just
#' like \code{\link[base]{rank}}.
#' 
#' @param x A list or vector of items to be sorted.
#' @param f A function on two arguments for comparing elements of \code{x}.
#' Returns \code{-1} if the first argument is less than the second, \code{1}
#' for the reverse, and \code{0} if they are equal or incomparable.
#' @param ... other arguments to \code{f}
#' @param random logical - should a random pivot be chosen? (this is
#' recommended) Otherwise middle element is used.
#' 
#' @return Returns an integer vector giving each element's position in the
#' order (minimal element(s) is 1, etc).
#' 
#' @section Warning: Output may not be consistent for certain partial orderings
#' (using random pivot), see example below.  All results will be consistent
#' with a total ordering which is itselft consistent with the true partial
#' ordering.
#' 
#' \code{f} is not checked to see that it returns a legitimate partial order,
#' so results may be meaningless if it is not.
#' @author Robin Evans
#' @seealso \code{\link[base]{order}}.
#' @references \url{https://en.wikipedia.org/wiki/Quicksort}.
#' @keywords arith optimize
#' @examples
#' 
#' set.seed(1)
#' quickSort(powerSet(1:3), f=subsetOrder)
#' quickSort(powerSet(1:3), f=subsetOrder)
#' # slightly different answers, but both correposnding
#' # to a legitimate total ordering.
#' 
#' @export quickSort
quickSort <-
function (x, f = greaterThan, ..., random = TRUE) 
{
  otherArgs <- match.call(expand.dots = FALSE)$`...`

  n = length(x)
  if (n < 2) 
    return(seq_len(n))
  if (n == 2) {
    if (length(otherArgs) >= 1) args <- c(list(x=x[[1]], y=x[[2]]), otherArgs)
    else args <- list(x=x[[1]], y=x[[2]])
    com = do.call(f, args)

    if (com == 0) 
      return(c(1, 1))
    else if (com == 1) 
      return(c(2, 1))
    else if (com == -1) 
      return(c(1, 2))
    else stop("How did we get here?")
  }
  if (random) 
    mid = sample(n, 1)
  else mid = ceiling(n/2)
  comp = numeric(n)
  comp[mid] = 2
  for (i in seq_len(n)[-mid]) {
    if (length(otherArgs) >= 1) args <- c(list(x=x[[i]], y=x[[mid]]), otherArgs)
    else args <- list(x=x[[i]], y=x[[mid]])

    # comp[i] = f(x[[i]], x[[mid]])
    comp[i] = do.call(f, args)
  }
  lu = Recall(x[comp == 1], f, ..., random)
  ld = Recall(x[comp == -1], f, ..., random)
  lm = Recall(x[comp == 0], f, ..., random)

  rank = numeric(n)
  rank[comp == -1] = ld
  rank[mid] = max(c(0, ld)) + 1
  rank[comp == 0] = lm + max(c(0, ld))
  rank[comp == 1] = lu + max(c(0, rank), na.rm = TRUE)

  return(rank)
}
