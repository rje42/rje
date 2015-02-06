#' Compute margin of a table faster
#' 
#' Computes the margin of a contingency table given as an array, by summing out
#' over the dimensions not specified.
#' 
#' With \code{order = TRUE} this is the same as the base function
#' \code{margin.table()}, but faster.
#' 
#' With \code{order = FALSE} the function is even faster, but the indices in
#' the margin are returned in their original order, regardless of the way they
#' are specified in \code{margin}.
#' 
#' \code{propTable()} returns a renormalized contingency table whose entries
#' sum to 1. It is equivalent to \code{prop.table()}, but faster.
#' 
#' @aliases marginTable propTable marginMatrix
#' @usage
#' marginTable(x, margin = NULL, order = TRUE) 
#' marginMatrix(x, margin, dim = NULL, incols = FALSE, order = FALSE) 
#' 
#' @param x a numeric array
#' @param margin integer vector giving margin to be calculated (1 for rows,
#' etc.)
#' @param order logical - should indices of output be ordered as in the vector
#' \code{margin}?  Defaults to \code{TRUE} for marginTable, \code{FALSE} for 
#' marginMatrix.
#' @param dim Integer vector containing dimensions of variables.  Assumed all
#' binary if not specified.
#' @param incols Logical specifying whether not the distributions are stored as
#' the columns in the matrix; assumed to be rows by default.
#' 
#' @return The relevant marginal table.  The class of \code{x} is copied to the
#' output table, except in the summation case.
#' @note Original functions are \code{\link[base]{margin.table}} and
#' \code{\link[base]{prop.table}}.
#' @keywords array
#' @examples
#' 
#' m <- matrix(1:4, 2)
#' marginTable(m, 1)
#' marginTable(m, 2)
#' 
#' propTable(m, 2)
#' 
#' # 3-way example
#' m <- array(1:8, rep(2,3))
#' marginTable(m, c(2,3))
#' marginTable(m, c(3,2))
#' marginTable(m, c(3,2), order=FALSE)
#' 
#' #' set.seed(2314)
#' # set of 10 2x2x2 probability distributions
#' x = rdirichlet(10, rep(1,8))
#' 
#' marginMatrix(x, c(1,3))
#' marginMatrix(t(x), c(1,3), incols=TRUE)
#' 
#' @export marginTable
marginTable <-
function (x, margin = NULL, order = TRUE) 
{
    k = length(dim(x))
    if (length(margin) > 0) 
        rmv = seq_len(k)[-margin]
    else return(sum(x))
    dx = dim(x)
    if (!is.double(x)) 
        x = as.double(x)
    out2 = .C("marginTable", x, as.integer(dx), as.integer(k), 
        as.integer(rmv), as.integer(length(rmv)), package = "rje")[[1]]
    if (order) {
        sm = sort.int(margin)
        kpm = dx[sm]
        out = out2[seq_len(prod(kpm))]
        dim(out) = kpm
        if (any(sm != margin))
            out = aperm.default(out, rank(margin))
        dimnames(out) = dimnames(x)[margin]        
    }
    else {
        kpm = dx[margin]
        out = out2[seq_len(prod(kpm))]
        dim(out) = kpm
        dimnames(out) = dimnames(x)[margin]
    }
    out
}


#' @export marginMatrix
marginMatrix <- function (x, margin, dim = NULL, incols = FALSE, order = FALSE) 
{
  d = 2 - incols
  if (is.null(dim)) 
    dim = rep(2, log2(dim(x)[d]))
  if (prod(dim) != dim(x)[d]) 
    stop("Dimensions do not match")
  if (length(margin) == 0) {
    out = if (incols) 
      matrix(colSums(x), nrow = 1)
    else matrix(rowSums(x), ncol = 1)
    return(out)
  }
  else if (length(margin) == length(dim)) {
    if (order) {
      patt = seq_len(prod(dim))
      dim(patt) = dim
      patt = aperm(patt, rank(margin))
      dim(patt) = NULL
      if (incols) 
        x = x[patt, , drop = FALSE]
      else x = x[, patt, drop = FALSE]
    }
    return(x)
  }
  else if (length(margin) > length(dim)) 
    stop("Margin specified is too long")
  idx = array(seq_len(dim(x)[d]), dim)
  rest = seq_along(dim)[-margin]
  combs = combinations(dim[rest]) + 1
  if (order) {
    patt = seq_len(prod(dim[margin]))
    dim(patt) = dim[sort.int(margin)]
    patt = aperm(patt, rank(margin))
    dim(patt) = NULL
  }
  if (incols) {
    out = matrix(0, ncol = ncol(x), nrow = prod(dim[margin]))
    for (i in seq(from = 1, to = nrow(combs))) {
      init = c(subtable(idx, rest, combs[i, ]))
      out = out + x[init, , drop = FALSE]
    }
    if (order) 
      out = out[patt, , drop = FALSE]
  }
  else {
    out = matrix(0, nrow = nrow(x), ncol = prod(dim[margin]))
    for (i in seq(from = 1, to = nrow(combs))) {
      init = c(subtable(idx, rest, combs[i, ]))
      out = out + x[, init, drop = FALSE]
    }
    if (order) 
      out = out[, patt, drop = FALSE]
  }
  out
}
