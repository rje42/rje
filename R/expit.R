#' Expit and Logit.
#' 
#' Functions to take the expit and logit of numerical vectors.
#' 
#' \code{logit} implements the usual logit function, which is \deqn{logit(x) =
#' \log\frac{x}{1-x},}{logit(x) = log(x/(1-x)),} and \code{expit} its inverse:
#' \deqn{expit(x) = \frac{e^x}{1+e^x}.}{expit(x) = e^x/(1+e^x).} It is assumed
#' that \code{logit(0) = -Inf} and \code{logit(1) = Inf}, and correspondingly
#' for \code{expit}.
#' 
#' @aliases expit logit
#' @param x vector of real numbers; for \code{logit} to return a sensible value
#' these should be between 0 and 1.
#' @return A real vector corresponding to the expits or logits of \code{x}
#' @section Warning : Choosing very large (positive or negative) values to
#' apply to \code{expit} may result in inaccurate inversion (see example
#' below).
#' @author Robin Evans
#' @keywords arith
#' @examples
#' 
#' x = c(5, -2, 0.1)
#' y = expit(x)
#' logit(y)
#' 
#' # Beware large values!
#' logit(expit(100))
# @name expit_logit
# NULL
# 
# @describeIn expit_logit expit function
#' @export
expit <-
function (x) 
{
    out = exp(x)/(1 + exp(x))
    out[x > 100] = 1
    out
}

#' @describeIn expit logit function
#' @export
logit <-
  function (x) 
  {
    no = (x < 0) | (x > 1)
    out = numeric(length(x))
    out[no] = NaN
    out[!no] = log(x[!no]/(1 - x[!no]))
    dim(out) = dim(x)
    out
  }
