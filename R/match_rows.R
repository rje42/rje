##' Match rows in a matrix with duplicates to set of unique values
##'
##' @param x matrix to match
##' @param y matrix with unique rows
##' @param nomatch value to insert when there is no match
##'
##' @export
match_rows <- function (x, y, nomatch=NA_integer_) {
  d <- ncol(x)
  if (ncol(y) != d) stop("Matrices must have matching number of columns")

  y2 <- unique(y)
  if (nrow(y2) < nrow(y)) stop("'y' contains duplicate rows")

  ## get a key that uniquely identifies rows in y
  cont <- TRUE
  while (cont) {
    key <- rnorm(d)
    vals_y <- y %*% key
    if (min(diff(sort.int(vals_y))) > 1e-8) cont <- FALSE
  }

  ## now apply that key to x, and match
  vals_x <- x %*% key
  match(vals_x, vals_y, nomatch=nomatch)
}
