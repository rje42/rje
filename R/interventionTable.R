#' Calculate interventional distributions.
#' 
#' Calculate interventional distributions from a probability table or matrix of
#' multivariate probability distributions.
#' 
#' This just divides the joint distribution \eqn{p(x)} by \eqn{p(v | c)}, where
#' \eqn{v} is \code{variables} and \eqn{c} is \code{condition}.
#' 
#' Under certain causal assumptions this is the interventional distribution
#' \eqn{p(x \,|\, do(v))} (i.e. if the direct causes of \eqn{v} are precisely
#' \eqn{c}.)
#' 
#' \code{intervention.table()} is identical to \code{interventionTable()}.
#' 
#' @aliases interventionTable intervention.table interventionMatrix
#' @param x An array of probabilities.
#' @param variables The margin for the intervention.
#' @param condition The dimensions to be conditioned upon.
#' @param dim Integer vector containing dimensions of variables.  Assumed all
#' binary if not specified.
#' @param incols Logical specifying whether not the distributions are stored as
#' the columns in the matrix; assumed to be rows by default.
#' @return A numerical array of the same dimension as \eqn{x}.
#' @author Robin Evans
#' @seealso \code{\link{conditionTable}}, \code{\link{marginTable}}
#' @references Pearl, J., \emph{Causality}, 2nd Edition. Cambridge University
#' Press, 2009.
#' @keywords arith
#' @examples
#' 
#' set.seed(413)
#' # matrix of distributions
#' p = rdirichlet(10, rep(1,16))
#' interventionMatrix(p, 3, 2)
#' 
#' # take one in an array
#' ap = array(p[1,], rep(2,4))
#' interventionTable(ap, 3, 2)
#' 
#' @export interventionTable
interventionTable <-
function (x, variables, condition) 
{
    tmp = conditionTable2(x, variables, condition)
    out = x/tmp
    out
}

#' @export intervention.table
intervention.table <- interventionTable
