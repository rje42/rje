#' Subset an array
#' 
#' More flexible calls of \code{[} on an array.
#' 
#' Essentially just allows more flexible calls of \code{[} on an array.
#' 
#' \code{subarray} requires the values for each dimension should be specified,
#' so for a \eqn{2 \times 2 \times 2}{2 x 2 x 2} array \code{x},
#' \code{subarray(x, list(1,2,1:2))} is just \code{x[1,2,1:2]}.
#' 
#' \code{subtable} allows unspecified dimensions to be retained automatically.
#' Thus, for example \code{subtable(x, c(2,3), list(1, 1:2))} is
#' \code{x[,1,1:2]}.
#' 
#' @aliases subarray subarray<- subtable subtable<-
#' @param x An array.
#' @param variables An integer vector containing the dimensions of \code{x} to
#' subset.
#' @param levels A list or vector containing values to retain.
#' @param drop Logical indicating whether dimensions with only 1 retained
#' should be dropped.  Defaults to \code{TRUE}.
#' @param value Value to assign to entries in table.
#' @return Returns an array of dimension \code{sapply(value, length)} if
#' \code{drop=TRUE}, otherwise \emph{specified} dimensions of size 1 are
#' dropped.  Dimensions which are unspecified in \code{subtable} are never
#' dropped.
#' @author Mathias Drton, Robin Evans
#' @seealso \code{\link{Extract}}
#' @keywords array
#' @examples
#' 
#' x = array(1:8, rep(2,3))
#' subarray(x, c(2,1,2)) == x[2,1,2]
#' 
#' x[2,1:2,2,drop=FALSE]
#' subarray(x, list(2,1:2,2), drop=FALSE)
#' 
#' subtable(x, c(2,3), list(1, 1:2))
#' 
#' @export subtable
subtable <-
function (x, variables, levels, drop = TRUE) 
{
    indexlist <- lapply(dim(x), seq_len)
    indexlist[variables] <- levels
    dims = dim(x)
    if (is.list(levels)) 
        dims[variables] = fsapply(levels, length)
    else dims[variables] = 1
    if (isTRUE(drop)) {
        dims = dims[!(seq_along(dims) %in% variables) | dims != 1]
        if (length(dims) == 1) dims = integer(0)
    }
    out = subarray(x, indexlist)
    if (length(dims) > 0) dim(out) = dims
    return(out)
}

#' @describeIn subtable Flexible subsetting
#' @export subarray
subarray <-
  function (x, levels, drop = TRUE) 
  {
    if (length(levels) != length(dim(x))) {
      stop("Array and indexlist are not compatible!")
    }
    args <- c(quote(x), levels, list(drop = drop))
    return(do.call("[", args))
  }

#' @describeIn subtable Assignment in a table
#' @export subtable<-
`subtable<-` <-
  function (x, variables, levels, value) 
  {
    indexlist <- lapply(dim(x), seq_len)
    indexlist[variables] <- levels
    dims = dim(x)
    dims[variables] = sapply(levels, length)
    out = x
    subarray(out, indexlist) <- value
    return(out)
  }

#' @describeIn subtable Assignment in an array
#' @export subarray<-
`subarray<-` <-
  function (x, levels, value) 
  {
    if (length(levels) != length(dim(x))) {
      stop("Array and indexlist are not compatible!")
    }
    args <- c(quote(x), levels, quote(value))
    return(do.call("[<-", args))
  }
