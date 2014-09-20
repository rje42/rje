#' Determine whether number is integral or not.
#' 
#' Checks whether a numeric value is integral, up to machine or other specified
#' prescision.
#' 
#' 
#' @param x numeric vector to be tested.
#' @param tol The desired precision.
#' @return A logical vector of the same length as \code{x}, containing the
#' results of the test.
#' @author Robin Evans
#' @keywords arith
#' @examples
#' 
#' x = c(0.5, 1, 2L, 1e-20)
#' is.wholenumber(x)
#' 
#' @export is.wholenumber
is.wholenumber <-
function (x, tol = .Machine$double.eps^0.5) 
abs(x - round(x)) < tol
