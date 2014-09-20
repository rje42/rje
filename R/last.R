#' Last element of a vector or list
#' 
#' Returns the last element of a list or vector.
#' 
#' Designed to be faster than using \code{tail()} or \code{rev()}, and cleaner
#' than writing \code{x[length(x)]}.
#' 
#' @param x a list or vector.
#' @return An object of the same type as \code{x} of length 1 (or empty if
#' \code{x} is empty).
#' @author Robin Evans
#' @seealso \code{\link[utils]{tail}}, \code{\link[base]{rev}}.
#' @keywords manip
#' @examples
#' 
#' last(1:10)
#' 
#' @export last
last <-
function (x) 
x[length(x)]
